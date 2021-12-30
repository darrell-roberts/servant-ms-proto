{-# LANGUAGE DeriveAnyClass #-}

module MSFramework.MongoUtil
  ( SanitizedQuery (..)
  , asMongoKey
  , filterBsonId
  , filterBsonNull
  , runAction
  , createMongoDBPool
  , closeMongoDBPool
  , checkAuth
  , documentToJson
  , runActionForConduit
  , sourceQuery
  ) where

import Conduit                        (ConduitT, MonadIO, yield)
import Control.Monad                  (unless)
import Control.Monad.Reader.Class     (MonadReader, ask)
import Control.Monad.Trans.Control    (MonadBaseControl)
import Data.Aeson                     (object, (.=))
import Data.Aeson                     qualified as A
import Data.Aeson.Types               (Pair)
import Data.Bool                      (bool)
import Data.Pool                      (createPool, destroyAllResources,
                                       withResource)
import Data.Text                      (Text, unpack)
import Database.MongoDB               (Action, Document, Failure,
                                       Field (value, (:=)), Host (Host),
                                       ObjectId, Query (Query, selection),
                                       Selection (..), Value (..), access, auth,
                                       close, closeCursor, connect, find,
                                       master, next)
import Database.MongoDB.Transport.Tls qualified as MTLS
import GHC.Conc                       (getNumProcessors)
import MSFramework.Types              (AppContext (..), Connection (..),
                                       ConnectionPool, ProgramOptions (..),
                                       Sanitize (..), maskSensitive)
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
  (MonadIO m, MonadReader AppContext m) ⇒
  Action IO a →
  m (Either Failure a)
runAction action = ask >>= \AppContext{mongoPool, options} ->
  liftIO $ try $
    withResource mongoPool $ \(Connection pipe) ->
      access pipe master (collectionName options) action

newtype SanitizedQuery
  = SanitizedQuery Query
  deriving (Show)

instance Sanitize SanitizedQuery where
  sanitize (SanitizedQuery q@Query{selection = s@Select{selector}}) =
    SanitizedQuery q{selection = s{selector = maskSensitive selector}}

newtype InvalidCredentials
  = InvalidCredentials String
  deriving (Exception, Show)

createMongoDBPool ∷ ProgramOptions → IO ConnectionPool
createMongoDBPool options = do
  cores <- getNumProcessors
  putStrLn $ "Setting number of stripes to " <> show cores
  hFlush stdout
  createPool
    (Connection <$> let host@(Host hostname port) = mongoHost options
                     in bool (connect host) (MTLS.connect hostname port) (mongoTls options))
    (\(Connection pipe) -> close pipe)
    (connectionPoolSize options)
    (connectionIdleTimeout options)
    cores

checkAuth ∷
  (MonadIO m, MonadBaseControl IO m) ⇒
  ConnectionPool →
  ProgramOptions →
  m ()
checkAuth mongoPool ProgramOptions{mongoUser, mongoPass, mongoDb} =
  withResource mongoPool $
    \(Connection pipe) -> do
      authOk <- access pipe master mongoDb $ auth mongoUser mongoPass
      unless authOk $
        throwIO $ InvalidCredentials $
          "Invalid credentials for user " <> unpack mongoUser

closeMongoDBPool ∷ AppContext → IO ()
closeMongoDBPool AppContext{mongoPool} = do
  putStrLn "Closing mongo db pooled connections"
  destroyAllResources mongoPool

-- | Convert a BSON document to a JSON document.
documentToJson ∷ Document → A.Value
documentToJson = object . fmap fieldToJson

-- | Convert a BSON field and value to a JSON field and value.
fieldToJson ∷ Field → Pair
fieldToJson (label := value) = mapValue value
  where
    mapValue v = case v of
      Float f   -> label .= f
      Bool b    -> label .= b
      UTC u     -> label .= u
      Int32 i32 -> label .= i32
      Int64 i64 -> label .= i64
      String s  -> label .= s
      _         -> label .= show v

-- | Couldn't run conduit and handler with AppM monad all at once.
runActionForConduit ∷ AppContext → Action IO a → IO a
runActionForConduit AppContext{mongoPool, options} action =
  withResource mongoPool $ \(Connection pipe) ->
    access pipe master (collectionName options) action

-- | A conduit mongodb producer. Runs a find query and streams documents.
sourceQuery ∷ AppContext → Query → ConduitT () Document IO ()
sourceQuery ctx query = do
  cursor <- liftIO $ runActionForConduit ctx $ find query
  let loop = do
        doc <- liftIO $ runActionForConduit ctx $ next cursor
        case doc of
          Nothing -> do
            liftIO $ runActionForConduit ctx $ closeCursor cursor
            pure ()
          Just d -> do
            yield d
            loop
  loop
