{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances #-}

module MSFramework.Types
  ( LogLevel(..)
  , LogMessage(..)
  , LogSettings(..)
  , ServerOptions(..)
  , MongoOptions(..)
  , ReqContext (..)
  , Connection (..)
  , ConnectionPool
  , Sanitize
  , HasServerOptions(..)
  , HasMongoOptions(..)
  , HasMongoConnectionPool(..)
  , HasRequestContext(..)
  , HasLogSettings(..)
  , maskSensitive
  , sanitize
  ) where

import Data.Aeson               (FromJSON, ToJSON, encode)
import Data.Int                 (Int64)
import Data.Pool                (Pool)
import Data.Text                (Text)
import Data.Time                (NominalDiffTime, UTCTime)
import Data.UUID                (UUID)
import Database.MongoDB         (Collection, Database, Field (..), Host,
                                 Password, Pipe, Username, Value (..))
import GHC.Generics             (Generic)
import MSFramework.Util         (maskShowFirstLast)
import Network.Wai.Handler.Warp (Port)
import System.Log.FastLogger    (LoggerSet, ToLogStr (toLogStr))


-- | Log levels for logger.
data LogLevel = Info | Warn | Error | Debug deriving
    ( Eq
    , FromJSON
    , Generic
    , Ord
    , Show
    , ToJSON
    )

-- | Server options parsed from command line.
data ServerOptions = ServerOptions
  { serverPort :: !Port
  , logLevel   :: !LogLevel
  , appName    :: !Text
  , sslCert    :: !FilePath
  , sslKey     :: !FilePath
  , hashPrefix :: !Text
  , jwk        :: !FilePath
  }
  deriving (Show)

-- | Mongo options parsed from command line.
data MongoOptions = MongoOptions
  { collectionName        :: !Collection
  , mongoHost             :: !Host
  , mongoUser             :: !Username
  , mongoPass             :: !Password
  , mongoDb               :: !Database
  , mongoTls              :: !Bool
  , mongoClientCert       :: !FilePath
  , mongoPrivateKey       :: !FilePath
  , mongoCaFile           :: !FilePath
  , connectionPoolSize    :: !Int
  , connectionIdleTimeout :: !NominalDiffTime
  }
  deriving (Show)

-- Type classes

class HasServerOptions a where
  serverOptions :: a -> ServerOptions

class HasMongoOptions a where
  mongoOptions :: a -> MongoOptions

class HasMongoConnectionPool a where
  mongoPool :: a -> ConnectionPool

class HasRequestContext a where
  requestContext :: a -> ReqContext

class HasLogSettings a where
  logSettings :: a -> LogSettings

-- | Environment for each request.
data ReqContext = ReqContext
  { requestId :: !(Maybe UUID)
  , props     :: ![(Text, Text)]
  }

-- | Mongo connection.
newtype Connection
  = Connection Pipe

-- | Mongo connection pool.
type ConnectionPool = Pool Connection

data LogSettings = LogSettings
  { logSet    :: !LoggerSet
  , errLogSet :: !LoggerSet
  }

-- | Log message with meta-data tags.
data LogMessage = LogMessage
  { message    :: !Text
  , timestamp  :: !UTCTime
  , level      :: !LogLevel
  , requestId  :: !(Maybe UUID)
  , appName    :: !Text
  , timeMillis :: !Int64
  , threadId   :: !Text
  }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- | For fast-logger
instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

-- | Type class for sanitization of sensitive values.
class Sanitize a where
  sanitize :: a -> a

-- | Sanitize `MongoOptions`.
instance Sanitize MongoOptions where
  sanitize options = options {mongoUser = "******", mongoPass = "*****"}

instance Sanitize ServerOptions where
  sanitize = id

-- | Sanitize sensitive mongo fields.
instance Sanitize [Field] where
  sanitize = maskSensitive

-- | Mask all known sensitive mongo field types.
maskSensitive ∷ [Field] → [Field]
maskSensitive = fmap check
  where
    maskedString (String s) = String $ maskShowFirstLast s
    maskedString _          = String "*****"
    check f@(name := value)
      | name == "email" = name := maskedString value
      | name == "name" = name := maskedString value
      | otherwise = f
