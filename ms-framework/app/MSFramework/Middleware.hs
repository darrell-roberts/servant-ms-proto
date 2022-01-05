module MSFramework.Middleware
  ( exceptionLogger
  , exceptionResponder
  , getRequestId
  , logInfo
  , logError
  , logDebug
  , logStartMiddleware
  , requestIdKey
  , requestIdMiddleware
  , startTimeKey
  , withReqContext
  , xRequestId
  ) where

import Control.Applicative   (Alternative ((<|>)))
import Control.Exception     (Exception (displayException), SomeException,
                              fromException)
import Control.Monad         (when, (>=>))
import Data.Aeson            (encode, object, (.=))
import Data.ByteString       (ByteString)
import Data.Foldable         (find)
import Data.Int              (Int64)
import Data.Maybe            (fromMaybe)
import Data.String           (fromString)
import Data.Text             (Text, pack, unpack)
import Data.Text.Encoding    (decodeUtf8)
import Data.Time             (getCurrentTime)
import Data.UUID             (UUID)
import Data.UUID.V4          (nextRandom)
import Data.Vault.Lazy       qualified as V
import MSFramework.Logger    (buildLogMessage)
import MSFramework.Types     (HasLogSettings (..), HasServerOptions (..),
                              LogLevel (Debug, Error, Info), LogSettings (..),
                              ReqContext (ReqContext), ServerOptions (..))
import MSFramework.Util      (millisSinceEpoch, showText)
import Network.HTTP.Types    (HeaderName, ResponseHeaders, Status (..),
                              status400)
import Network.Wai           (Application, Middleware,
                              Request (rawPathInfo, requestMethod), Response,
                              mapResponseHeaders, requestHeaders,
                              responseHeaders, responseLBS, responseStatus,
                              vault)
import System.IO.Unsafe      (unsafePerformIO)
import System.Log.FastLogger (pushLogStrLn)
import Text.Read             (readMaybe)
import UnliftIO              (throwIO, tryAny)

xRequestId ∷ HeaderName
xRequestId = "X-Request-Id"

logger ∷
  (HasLogSettings a, HasServerOptions a) ⇒
  LogLevel →
  a →
  Maybe UUID →
  Text →
  IO ()
logger level env uuid message = do
  let ServerOptions{appName} = serverOptions env
  let LogSettings{logSet} = logSettings env
  logStr <- buildLogMessage level appName uuid message
  pushLogStrLn logSet logStr

logInfo, logError, logDebug ∷
  (HasLogSettings a, HasServerOptions a) ⇒
  a  →
  Maybe UUID →
  Text →
  IO ()
logInfo = logger Info
logError = logger Error
logDebug = logger Debug

-- https://www.yesodweb.com/blog/2015/10/using-wais-vault

startTimeKey ∷ V.Key Int64
startTimeKey = unsafePerformIO V.newKey
{-# NOINLINE startTimeKey #-}

requestIdKey ∷ V.Key UUID
requestIdKey = unsafePerformIO V.newKey
{-# NOINLINE requestIdKey #-}

getRequestId ∷ Request → Maybe UUID
getRequestId req = V.lookup requestIdKey (vault req)

logStartMiddleware ∷ (HasLogSettings a, HasServerOptions a) ⇒ a → Middleware
logStartMiddleware ctx application req responseFunc = do
  logDebug ctx (getRequestId req) "log start middleware"
  now <- millisSinceEpoch <$> getCurrentTime
  let v = V.insert startTimeKey now $ vault req
      req' = req{vault = v}
  logInfo ctx (getRequestId req) $
    "Request: "
      <> showText (requestMethod req)
      <> " "
      <> showText (rawPathInfo req)
  application req' responseFunc

getElapsedTime ∷ Request → Int64 → Int64
getElapsedTime req startTime = case V.lookup startTimeKey (vault req) of
    Nothing -> -1
    Just t  -> startTime - t

{-|
  Attempt to get a request id header and pass it through,
  otherwise create a new UUID and pass it through.
-}
requestIdMiddleware ∷ (HasLogSettings a, HasServerOptions a) ⇒ a → Middleware
requestIdMiddleware ctx application req responseFunc = do
  requestId <- maybe nextRandom pure $ getRequestIdHeader req
  let v = V.insert requestIdKey requestId $ vault req
      req' = req{vault = v}
      reqIdText = fromString (show requestId)
  logDebug ctx (Just requestId) $ "passing request id " <> showText requestId
  application req' (responseFunc . mapResponseHeaders (addRequestIdheader reqIdText))
  where
    addRequestIdheader ∷ ByteString → ResponseHeaders → ResponseHeaders
    addRequestIdheader reqIdText headers = (xRequestId, reqIdText) : headers

    getRequestIdHeader ∷ Request → Maybe UUID
    getRequestIdHeader =
      fmap (unpack . decodeUtf8 . snd) . find (\(name, _) -> name == xRequestId)
        . requestHeaders >=> readMaybe

showReqUrl ∷ Request → Text
showReqUrl req =
    showText (requestMethod req)
      <> " "
      <> showText (rawPathInfo req)

{-|
  Generalized exception wrapper allowing the
  requesId to be passed to the exception responder function
  that does not have access to the `Request` `Vault`.
-}
data ApiCallException = ApiCallException
  { requestId         :: !(Maybe UUID)
  , originalException :: !SomeException
  }
  deriving (Show)

instance Exception ApiCallException where
  displayException ApiCallException{originalException} =
    displayException originalException

getRequestIdFromApiException ∷ SomeException → Maybe ByteString
getRequestIdFromApiException = fromException >=> \ApiCallException{requestId} ->
  fmap (fromString . show) requestId

exceptionResponder ∷ SomeException → Response
exceptionResponder e = responseLBS status400 headers (encode errorObj)
  where
    errorObj = object ["error" .= displayException e]
    defaultHeaders = [("Content-Type", "application/json")]
    headers = case getRequestIdFromApiException e of
      Just bs -> (xRequestId, bs) : defaultHeaders
      Nothing -> defaultHeaders

{-|
  This adds some post handling to the middleware stack
  by checking for errors and handling error logging and
  re-throwing `Exception`s.
-}
exceptionLogger ∷ (HasLogSettings a, HasServerOptions a) ⇒ a → Middleware
exceptionLogger ctx application req resFunc = do
  logDebug ctx (getRequestId req) "exception logger middleware"
  result <- tryAny $ application req $ \res -> do
    -- Parsing errors in Servant won't raise an exception.
    -- This is a way to get those errors to log.
    when (statusMessage (responseStatus res) /= "OK") $ do
      let errorMsg = fmap ((" Error message: " <>) . decodeUtf8 . snd) $
            find (\(name, _) -> name == "X-Error") $
            responseHeaders res
      logError ctx (getRequestId req) $
        "Request: "
          <> showReqUrl req
          <> " Failed with Status "
          <> showText (responseStatus res)
          <> showText (fromMaybe "" (errorMsg <|> Just "No error message"))
    resFunc res

  now <- millisSinceEpoch <$> getCurrentTime
  let elapsedTime = getElapsedTime req now
  case result of
    Left e  -> do
      logError ctx (getRequestId req) $
        "Error on Request: "
          <> showReqUrl req
          <> " "
          <> pack (displayException e)
          <> " completed in "
          <> showText elapsedTime
          <> " ms"
      throwIO $ ApiCallException (getRequestId req) e
    Right r -> do
      logInfo ctx (getRequestId req) $
        "Request: "
          <> pack (show $ requestMethod req)
          <> " "
          <> pack (show $ rawPathInfo req)
          <> " completed in "
          <> pack (show elapsedTime)
          <> " ms"
      pure r

{-|
  Pass per request environment to servant so it can be
  used in the Reader Monad which the handlers and persistence
  functions run under.
-}
withReqContext ∷ (ReqContext → Application) → Application
withReqContext app req sendResponse = app reqContext req sendResponse
  where
    reqContext = ReqContext (getRequestId req) []
