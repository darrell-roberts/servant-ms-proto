module MSFramework.Logger
  ( buildLogMessage
  , logDebug
  , logError
  , logInfo
  , logWarn
  , toJsonForLog
  ) where

import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Aeson                 (ToJSON, encode)
import Data.Text                  (Text)
import Data.Text.Lazy             (toStrict)
import Data.Text.Lazy.Encoding    (decodeUtf8)
import Data.Time                  (getCurrentTime)
import Data.UUID                  (UUID)
import MSFramework.Data           (AppContext (..), LogLevel (..),
                                   LogMessage (..), ProgramOptions (..))
import MSFramework.Util           (millisSinceEpoch)
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
  pure $ toLogStr $ LogMessage
    { message
    , timestamp
    , level
    , requestId
    , appName
    , timeMillis = epoch
    }

logger ∷
  (MonadReader AppContext m, MonadIO m) ⇒
  LogLevel →
  Maybe UUID →
  Text →
  m ()
logger level requestId message = do
  AppContext{logSet, errLogSet, options} <- ask
  let ProgramOptions{appName, logLevel} = options
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
  (MonadReader AppContext m, MonadIO m) ⇒
  Maybe UUID →
  Text → m ()
logDebug = logger Debug
logInfo = logger Info
logWarn = logger Warn
logError = logger Error

toJsonForLog ∷ ToJSON a ⇒ a → Text
toJsonForLog = toStrict . decodeUtf8 . encode
