{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module MSFramework.Data
  ( AppContext (..)
  , AppM
  , LogLevel (..)
  , LogMessage (..)
  , ProgramOptions (..)
  , Sanitize
  , sanitize
  ) where

import Control.Monad.Reader     (ReaderT)
import Data.Aeson               (FromJSON, ToJSON, encode)
import Data.Int                 (Int64)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.UUID                (UUID)
import Database.MongoDB         (Collection, Database, Host, Password, Pipe,
                                 Username)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (Port)
import Servant                  (Handler)
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
data ProgramOptions
   = ProgramOptions
     { mongoHost      :: !Host
     , serverPort     :: !Port
     , collectionName :: !Collection
     , logLevel       :: !LogLevel
     , appName        :: !Text
     , mongoUser      :: !Username
     , mongoPass      :: !Password
     , mongoDb        :: !Database
     }
  deriving (Show)

-- | Application context.
data AppContext
   = AppContext
     { mongoPipe :: !Pipe
     , options   :: !ProgramOptions
     , logSet    :: !LoggerSet
     , errLogSet :: !LoggerSet
     }

-- | Application monad.
type AppM = ReaderT AppContext Handler

-- | Log message with meta-data tags.
data LogMessage
   = LogMessage
     { message    :: !Text
     , timestamp  :: !UTCTime
     , level      :: !LogLevel
     , requestId  :: !(Maybe UUID)
     , appName    :: !Text
     , timeMillis :: !Int64
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
