module Main (main) where

import Control.Exception           (Exception (displayException), IOException,
                                    catch)
import Control.Monad               (void, when)
import Data.Default.Class          (def)
import Data.Maybe                  (fromMaybe, isNothing)
import Data.Text.Lazy              (toStrict)
import Data.Text.Lazy.Encoding     (decodeUtf8)
import Data.X509.CertificateStore  (readCertificateStore)
import Database.MongoDB            (Host (..))
import MSFramework.Arguments       (parseMongoOptions, parserServerOptions)
import MSFramework.Middleware      (exceptionLogger, exceptionResponder,
                                    logDebug, logError, logInfo,
                                    logStartMiddleware, requestIdMiddleware,
                                    withReqContext)
import MSFramework.MongoUtil       (checkConnection, closeMongoDBPool,
                                    createMongoDBPool)
import MSFramework.Types           (HasMongoOptions (..), HasServerOptions (..),
                                    LogSettings (LogSettings),
                                    MongoOptions (..), ReqContext (..),
                                    Sanitize (sanitize), ServerOptions (..))
import MSFramework.Util            (showText)
import Network.TLS                 (ClientHooks (onCertificateRequest),
                                    ClientParams (clientHooks, clientShared, clientSupported),
                                    Credentials (Credentials),
                                    Shared (sharedCAStore, sharedCredentials),
                                    Supported (supportedCiphers),
                                    credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher    (ciphersuite_default)
import Network.Wai.Handler.Warp    (Settings, defaultSettings,
                                    setGracefulShutdownTimeout,
                                    setInstallShutdownHandler,
                                    setOnExceptionResponse, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Gzip (gzip)
import Options.Applicative         (Parser, ParserInfo, execParser, fullDesc,
                                    helper, info, progDesc, (<**>))

import Data.Aeson                  (encode)
import Data.Text                   (pack)
import System.Exit                 (exitFailure)
import System.Log.FastLogger       (defaultBufSize, newStderrLoggerSet,
                                    newStdoutLoggerSet)
import System.Posix.Signals        (Handler (CatchOnce), addSignal,
                                    emptySignalSet, installHandler, sigINT,
                                    sigTERM)
import UserService.Security        (createJwt, getJWK)
import UserService.Server          (app)
import UserService.Types           (Role (..), UserAppOptions (..),
                                    UserMsAppContext (..))

-- | Compose program options used by this application.
parserOptions ∷ Parser UserAppOptions
parserOptions = UserAppOptions
  <$> parserServerOptions
  <*> parseMongoOptions

programOpts ∷ ParserInfo UserAppOptions
programOpts =
    info
    (parserOptions  <**> helper)
    (fullDesc <> progDesc "Run Haskell Servant/Wai User Micro-Service")

-- | Wai server settings.
warpSettings ∷ UserMsAppContext → Settings
warpSettings ctx = setPort (port ctx) $
  setGracefulShutdownTimeout (Just 10) $
  setInstallShutdownHandler shutdownHanlder $
  setOnExceptionResponse exceptionResponder defaultSettings
  where
    port = serverPort . serverOptions
    shutdownHanlder closeSocket = void $ installHandler sigTERM
        (CatchOnce $ do
          logError ctx Nothing "Term / Interrupt signal sent"
          closeMongoDBPool ctx
          closeSocket
        )
        (Just $ sigINT `addSignal` emptySignalSet)

-- | Start a Wai / servant application with middleware.
startServer ∷ UserMsAppContext → IO ()
startServer ctx = do
  logInfo ctx Nothing $ "Running with options: "
    <> showText (sanitize $ serverOptions ctx)
    <> " "
    <> showText (sanitize $ mongoOptions ctx)

  logInfo ctx Nothing $ "Server starting on port "
    <> showText (serverPort $ serverOptions ctx)

  key <- getJWK (jwk . _serverOptions . options $ ctx)
  jwts <- sequence <$> sequence [createJwt key Admin, createJwt key NormalUser]

  case jwts of
    Left e -> logError ctx Nothing $ showText e
    Right keys -> do
      logDebug ctx Nothing $ "JWK: " <> toStrict (decodeUtf8 $ encode key)
      case keys of
        [adminJwt, userJwt] -> do
          logDebug ctx Nothing $ "admin: " <> toStrict (decodeUtf8 adminJwt)
          logDebug ctx Nothing $ "user: " <> toStrict (decodeUtf8 userJwt)

        _ -> logError ctx Nothing "Wrong number of keys" >> exitFailure

      runTLS tlsOpts (warpSettings ctx) $
        requestIdMiddleware ctx $
        logStartMiddleware ctx $
        exceptionLogger ctx $
        gzip def $
        withReqContext (app key . addReqId ctx)

      where
        addReqId aCtx rCtx = aCtx {reqContext = rCtx}
        tlsOpts = tlsSettings (sslCert $ serverOptions ctx) (sslKey $ serverOptions ctx)

-- | Setup TLS client parameters. For mongodb we have a client certificate.
getTLSClientParams ∷ MongoOptions → IO ClientParams
getTLSClientParams pOpts = do
  let (Host host _) = mongoHost pOpts
      MongoOptions{mongoClientCert, mongoPrivateKey, mongoCaFile} = pOpts

  creds <- credentialLoadX509 mongoClientCert mongoPrivateKey
  caStore <- readCertificateStore mongoCaFile

  when (isNothing caStore) $ error ("Failed to load CaFile: " <> mongoCaFile)

  case creds of
    Left e -> error $ "Failed to load certificate " <> e
    Right c -> do
        let params = defaultParamsClient host ""
            cShared = clientShared params
            caShared = sharedCAStore cShared
            (Credentials sharedCred) = sharedCredentials cShared

        pure params
          { clientSupported = def { supportedCiphers = ciphersuite_default }
          , clientHooks = def { onCertificateRequest = \_ -> pure $ Just c }
          , clientShared = cShared { sharedCredentials = Credentials (c : sharedCred)
                                   , sharedCAStore = fromMaybe caShared caStore
                                   }
          }

main ∷ IO ()
main = do
  o@UserAppOptions{_serverOptions, _mongoOptions} <- execParser programOpts

  clientParams <- Just <$> getTLSClientParams _mongoOptions
  pool <- createMongoDBPool clientParams _mongoOptions

  logSettings <- LogSettings <$> newStdoutLoggerSet defaultBufSize
                             <*> newStderrLoggerSet defaultBufSize

  let env = UserMsAppContext pool o logSettings (ReqContext Nothing [])

  -- Make sure we can connect to database (mongodb)
  checkConnection env `catch` \e -> do
    logError env Nothing $ "Failed to connect to mongodb: " <>
      pack (displayException (e :: IOException))
    exitFailure

  startServer env
