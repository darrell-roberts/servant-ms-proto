{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia, FlexibleInstances #-}

module MSFramework.Types
  ( AppContext (..)
  , AppM(..)
  , LogLevel (..)
  , LogMessage (..)
  , ProgramOptions (..)
  , ReqContext (..)
  , Connection (..)
  , ConnectionPool
  , Sanitize
  , maskSensitive
  , sanitize
  ) where

import Control.Monad.Except     (MonadError, MonadIO)
import Control.Monad.Reader     (MonadReader, ReaderT)
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
import Servant                  (Handler, ServerError)
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

-- | Program options parsed from command line.
data ProgramOptions = ProgramOptions
  { serverPort            :: !Port
  , logLevel              :: !LogLevel
  , appName               :: !Text
  , collectionName        :: !Collection
  , mongoHost             :: !Host
  , mongoUser             :: !Username
  , mongoPass             :: !Password
  , mongoDb               :: !Database
  , mongoTls              :: !Bool
  , connectionPoolSize    :: !Int
  , connectionIdleTimeout :: !NominalDiffTime
  , sslCert               :: !FilePath
  , sslKey                :: !FilePath
  }
  deriving (Show)

-- | Environment for each request.
data ReqContext = ReqContext
  { requestId :: !(Maybe UUID)
  , props     :: ![(Text, Text)]
  }

newtype Connection
  = Connection Pipe
type ConnectionPool = Pool Connection

-- | Application context.
data AppContext = AppContext
  { mongoPool  :: !ConnectionPool
  , options    :: !ProgramOptions
  , logSet     :: !LoggerSet
  , errLogSet  :: !LoggerSet
  , reqContext :: !ReqContext
  }

-- type AppM = ReaderT AppContext Handler
-- | Application monad.
newtype AppM a
  = AppM (ReaderT AppContext Handler a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError ServerError
    , MonadIO
    , MonadReader AppContext
    )
    via ReaderT AppContext Handler

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

-- | Sanitize `ProgramOptions`.
instance Sanitize ProgramOptions where
  sanitize options = options {mongoUser = "******", mongoPass = "*****"}

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
