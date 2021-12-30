{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies #-}

module MSFramework.Servant
  ( errorFormatters
  , sendJSONError
  ) where

import Control.Monad.Error.Class (MonadError)
import Data.Aeson                (KeyValue ((.=)), encode, object)
import Data.ByteString.Lazy      qualified as BL
import Data.String               (fromString)
import Servant                   (ErrorFormatter, ErrorFormatters (..),
                                  ServerError (errBody, errHeaders),
                                  defaultErrorFormatters, err400, err500,
                                  throwError)

{--
  https://github.com/haskell-servant/servant/issues/719#issuecomment-476465681

  I have middleware that stores a requestId in vault for each request. A dynamic
  logger is provided with meta-data setup for each request.
-}
{--
-- | Alias for logger function that runs with Reader Transformer.
type LogWithEnv = ∀ m. (MonadReader AppContext m, MonadIO m) ⇒ Text -> m ()

-- | Provides functions for logging within a Servant Handler or Mongo Persistence module.
data ReqLogger
   = ReqLogger
     { logDebug :: LogWithEnv
     , logInfo  :: LogWithEnv
     , logError :: LogWithEnv
     , logWarn  :: LogWithEnv
     }

-- | Using 'ReqLogger' combinator does not impact the client.
instance HasClient m api => HasClient m (ReqLogger :> api) where
  type Client m (ReqLogger :> api) = Client m api

  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

-- | Combinator for providing the request logger to each handler.
instance (HasServer api context) ⇒ HasServer (ReqLogger :> api) context where
  type ServerT (ReqLogger :> api) m = ReqLogger -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver getReqLogger)
   where
    getReqLogger req =
      let r = requestIdKey `V.lookup` vault req
       in ReqLogger
            { logDebug = Logger.logDebug r
            , logInfo = Logger.logInfo r
            , logError = Logger.logError r
            , logWarn = Logger.logWarn r
            }

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s
-}
-- | Raise an error with an http 500 response code and JSON formatted error message.
sendJSONError ∷ MonadError ServerError m ⇒ BL.ByteString → m a
sendJSONError msg =
  throwError $
    err500
      { errBody = "{\"error\": " <> msg <> "}"
      , errHeaders = [("Content-Type", "application/json")]
      }

{-
  Below are formatters for errors that can occur with Servant parsing the request.
  The customer error formatter handles sending the response as a JSON response and
  a customer header to allow logging the original error in WAI middleware.

  https://docs.servant.dev/en/stable/cookbook/custom-errors/CustomErrors.html
-}

-- | Return errors as JSON.
errorFormatter ∷ ErrorFormatter
errorFormatter typeRep _ err =
  err400
    { errBody = encode jsonResp
    , errHeaders = defaultHeaders
    }
  where
    jsonResp = object ["error" .= err , "combinator" .= show typeRep]
    defaultHeaders = [ ("Content-Type", "application/json")
                     , ("X-Error", fromString $ err <> " " <> show typeRep)
                     ]

-- | Servant error formatters.
errorFormatters ∷ ErrorFormatters
errorFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = errorFormatter
  , urlParseErrorFormatter = errorFormatter
  , headerParseErrorFormatter = errorFormatter
  }
