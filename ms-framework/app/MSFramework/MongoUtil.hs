{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module MSFramework.MongoUtil
  ( SanitizedQuery (..)
  , asMongoKey
  , filterBsonId
  , filterBsonNull
  , runAction
  , createMongoDBPool
  , closeMongoDBPool
  , documentToJson
  , runMongoActionInIO
  , sourceQuery
  , checkConnection
  ) where

import Conduit                        (ConduitT, MonadIO, yield)
import Control.Monad                  (unless)
import Control.Monad.Reader.Class     (MonadReader, reader)
import Data.Aeson                     (object, (.=))
import Data.Aeson                     qualified as A
import Data.Aeson.Key                 (fromText)
import Data.Aeson.Types               (Pair)
import Data.Bool                      (bool)
import Data.Pool                      (createPool, destroyAllResources,
                                       withResource)
import Data.Text                      (Text, unpack)
import Database.MongoDB               (Action, Document, Failure,
                                       Field (value, (:=)), Host (Host),
                                       ObjectId, Query (Query, selection),
                                       Select, Selection (..), Value (..),
                                       access, allCollections, auth, close,
                                       closeCursor, connect, find, master, next)
import Database.MongoDB.Transport.Tls qualified as MTLS
import GHC.Conc                       (getNumProcessors)
import MSFramework.Types              (Connection (..), ConnectionPool,
                                       HasMongoConnectionPool (..),
                                       HasMongoOptions (..), MongoOptions (..),
                                       Sanitize (..), maskSensitive)
import Network.TLS                    (ClientParams)
import Text.Read                      (readMaybe)
import UnliftIO                       (Exception, MonadIO (..), hFlush, liftIO,
                                       stdout, throwIO, try)

-- | Convert BSON key from Text to ObjectId Type.
asMongoKey ∷ Text → Maybe ObjectId
asMongoKey = readMaybe . unpack

-- | Remove null Bson fields
filterBsonNull ∷ [Field] → [Field]
filterBsonNull = filter (\(:=){value} -> value /= Null)

-- | Remove Bson ObjectId types
filterBsonId ∷ [Field] → [Field]
filterBsonId = filter (\(:=){value} -> f value)
  where f (ObjId _) = False
        f _         = True

{-|
  Run a mongo query using settings from reader environment. `Failure`
  can be pattern matched for different error reasons:
  https://hackage.haskell.org/package/mongoDB-2.7.1.1/docs/Database-MongoDB-Query.html#t:Failure
-}
runAction ∷
  ( HasMongoOptions env
  , HasMongoConnectionPool env
  , MonadIO m
  , MonadReader env m) ⇒
  Action IO a →
  m (Either Failure a)
runAction action = do
  MongoOptions{mongoDb} <- reader mongoOptions
  p <- reader mongoPool
  liftIO $ try $
    withResource p $ \(Connection pipe) ->
      access pipe master mongoDb action

newtype SanitizedQuery
  = SanitizedQuery Query
  deriving (Eq, Select, Show)
    via Query

instance Sanitize SanitizedQuery where
  sanitize (SanitizedQuery q@Query{selection = s@Select{selector}}) =
    SanitizedQuery q{selection = s{selector = maskSensitive selector}}

newtype InvalidCredentials
  = InvalidCredentials String
  deriving (Exception, Show)

-- | Create a connection pool of mongodb connections.
createMongoDBPool ∷ ClientParams → MongoOptions → IO ConnectionPool
createMongoDBPool cparams mopts = do
  cores <- getNumProcessors
  hFlush stdout
  let host@(Host hostname port) = mongoHost mopts
      MongoOptions{mongoUser, mongoPass, mongoDb, mongoTls} = mopts
  createPool
    (do
       con <- bool (connect host) (MTLS.connectWithTlsParams cparams hostname port) mongoTls
       authOk <- access con master mongoDb $ auth mongoUser mongoPass
       unless authOk $
        throwIO $ InvalidCredentials $
          "Invalid credentials for user " <> unpack mongoUser
       pure $ Connection con
    )
    (\(Connection pipe) -> close pipe)
    (connectionPoolSize mopts)
    (connectionIdleTimeout mopts)
    cores

-- | Destroy mongodb connection pool.
closeMongoDBPool ∷ HasMongoConnectionPool a ⇒ a → IO ()
closeMongoDBPool env = do
  putStrLn "Closing mongo db pooled connections"
  destroyAllResources (mongoPool env)

-- | Convert a BSON document to a JSON document.
documentToJson ∷ Document → A.Value
documentToJson = object . fmap fieldToJson

-- | Convert a BSON field and value to a JSON field and value.
fieldToJson ∷ Field → Pair
fieldToJson (label := value) = mapValue value
  where
    key = fromText label
    mapValue v = case v of
      Float f   -> key .= f
      Bool b    -> key .= b
      UTC u     -> key .= u
      Int32 i32 -> key .= i32
      Int64 i64 -> key .= i64
      String s  -> key .= s
      _         -> key .= show v

-- | Couldn't run conduit and handler with AppM monad all at once.
runMongoActionInIO ∷
  (HasMongoConnectionPool env, HasMongoOptions env) ⇒
  env →
  Action IO a →
  IO a
runMongoActionInIO env action =
  withResource (mongoPool env) $ \(Connection pipe) ->
    access pipe master (mongoDb $ mongoOptions env) action

-- | A conduit mongodb producer. Runs a find query and streams documents.
sourceQuery ∷
  (HasMongoConnectionPool env, HasMongoOptions env) ⇒
  env →
  Query →
  ConduitT () Document IO ()
sourceQuery ctx query = do
  cursor <- liftIO $ runMongoActionInIO ctx $ find query
  let loop = do
        doc <- liftIO $ runMongoActionInIO ctx $ next cursor
        case doc of
          Nothing -> do
            liftIO $ runMongoActionInIO ctx $ closeCursor cursor
            pure ()
          Just d -> do
            yield d
            loop
  loop

checkConnection ∷
  (HasMongoConnectionPool env, HasMongoOptions env) ⇒
  env →
  IO ()
checkConnection env = do
  _collections <- runMongoActionInIO env allCollections
  pure ()
