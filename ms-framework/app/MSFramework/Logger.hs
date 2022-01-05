module MSFramework.Logger
  ( buildLogMessage
  , logDebug
  , logError
  , logInfo
  , logWarn
  , toJsonForLog
  , toJsonSanitized
  ) where

import Control.Concurrent         (myThreadId)
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (reader))
import Data.Aeson                 (ToJSON, encode)
import Data.Text                  (Text)
import Data.Text.Lazy             (toStrict)
import Data.Text.Lazy.Encoding    (decodeUtf8)
import Data.Time                  (getCurrentTime)
import Data.UUID                  (UUID)
import MSFramework.Types          (HasLogSettings (..),
                                   HasRequestContext (requestContext),
                                   HasServerOptions (serverOptions),
                                   LogLevel (..), LogMessage (..),
                                   LogSettings (..), ReqContext (..),
                                   Sanitize (sanitize), ServerOptions (..))
import MSFramework.Util           (millisSinceEpoch, showText)
import System.Log.FastLogger      (LogStr, ToLogStr (toLogStr), pushLogStrLn)

buildLogMessage ∷
  LogLevel →
  Text →
  Maybe UUID →
  Text →
  IO LogStr
buildLogMessage level appName requestId message = do
  timestamp <- liftIO getCurrentTime
  let epoch = millisSinceEpoch timestamp
  threadId <- showText <$> myThreadId
  pure $ toLogStr $ LogMessage
    { message
    , timestamp
    , level
    , requestId
    , appName
    , timeMillis = epoch
    , threadId
    }

logger ∷
  ( HasLogSettings env
  , HasRequestContext env
  , HasServerOptions env
  , MonadReader env m
  , MonadIO m
  ) ⇒
  LogLevel →
  Text →
  m ()
logger level message = do
  ServerOptions{appName, logLevel} <- reader serverOptions
  ReqContext{requestId} <- reader requestContext
  LogSettings{logSet, errLogSet} <- reader logSettings

  logMessage <- liftIO $ buildLogMessage level appName requestId message
  when (isLevelEnabled level logLevel) $
    liftIO $
      case level of
        Error -> pushLogStrLn errLogSet logMessage
        _     -> pushLogStrLn logSet logMessage

isLevelEnabled ∷ LogLevel → LogLevel → Bool
isLevelEnabled requestedLevel enabledLevel =
  requestedLevel `elem` levels enabledLevel
 where
  levels ∷ LogLevel → [LogLevel]
  levels Debug = [Error, Info, Warn, Debug]
  levels Warn  = [Error, Info, Warn]
  levels Info  = [Error, Info]
  levels Error = [Error]

logDebug, logInfo, logWarn, logError ::
  ( HasLogSettings env
  , HasRequestContext env
  , HasServerOptions env
  , MonadReader env m
  , MonadIO m
  ) ⇒
  Text →
  m ()
logDebug = logger Debug
logInfo = logger Info
logWarn = logger Warn
logError = logger Error

toJsonForLog ∷ ToJSON a ⇒ a → Text
toJsonForLog = toStrict . decodeUtf8 . encode

toJsonSanitized ∷ (ToJSON a, Sanitize a) ⇒ a → Text
toJsonSanitized = toJsonForLog . sanitize
