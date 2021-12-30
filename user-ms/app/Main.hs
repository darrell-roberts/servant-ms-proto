module Main where

import Control.Monad               (void)
import Data.Time.Clock             (NominalDiffTime)
import Database.MongoDB            (Host (..), PortID (PortNumber),
                                    readHostPortM)
import MSFramework.Middleware      (exceptionLogger, exceptionResponder,
                                    logError, logInfo, logStartMiddleware,
                                    requestIdMiddleware, withReqContext)
import MSFramework.MongoUtil       (checkAuth, closeMongoDBPool,
                                    createMongoDBPool)
import MSFramework.Types           (AppContext (..), LogLevel (..),
                                    ProgramOptions (..), ReqContext (..),
                                    Sanitize (sanitize))
import MSFramework.Util            (showText)
import Network.Socket              qualified as N
import Network.Wai.Handler.Warp    (Settings, defaultSettings,
                                    setGracefulShutdownTimeout,
                                    setInstallShutdownHandler,
                                    setOnExceptionResponse, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Gzip (def, gzip)
import Options.Applicative         (Parser, ParserInfo, ReadM, auto,
                                    eitherReader, execParser, fullDesc, help,
                                    helper, info, long, metavar, option,
                                    progDesc, short, showDefault, strOption,
                                    switch, value, (<**>))
import System.Log.FastLogger       (defaultBufSize, newStderrLoggerSet,
                                    newStdoutLoggerSet)
import System.Posix.Signals        (Handler (CatchOnce), addSignal,
                                    emptySignalSet, installHandler, sigINT,
                                    sigTERM)
import Text.Read                   (readMaybe)
import UserService.Server          (app)

parseMongoOptions ∷ ReadM Host
parseMongoOptions = eitherReader $ \s ->
  let mHost = readHostPortM s
   in maybe (Left $ "invalid host port " <> s) Right mHost

parseLogLevel ∷ ReadM LogLevel
parseLogLevel = eitherReader $ \case
  "debug" -> Right Debug
  "info"  -> Right Info
  "error" -> Right Error
  "warn"  -> Right Warn
  s       -> Left $ "Invalid argument: " <> s

parsePortNumber ∷ ReadM PortID
parsePortNumber = eitherReader $ \s ->
    let pn = readMaybe s :: Maybe N.PortNumber
     in case pn of Just n  -> Right $ PortNumber n
                   Nothing -> Left $ "Invalid port number: " <> s

parseArguments ∷ Parser ProgramOptions
parseArguments =
  ProgramOptions
    <$> option auto
        ( long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port number for API server"
        )
    <*> option
        parseLogLevel
        ( long "logLevel"
            <> help "Logging level"
            <> metavar "LEVEL"
            <> showDefault
            <> value Info
        )
    <*> strOption
        ( long "appName"
            <> help "Application Name"
            <> metavar "APPNAME"
        )
    <*> strOption
        ( long "collectionName"
            <> help "MongoDB Collection Name"
            <> metavar "NAME"
        )
    <*> option parseMongoOptions
        ( long "mongoHost"
            <> help "MongoDB hostname and port number"
            <> metavar "HOST:PORT"
        )
    <*> strOption
        ( long "mongoUser"
            <> help "Mongo user name"
            <> metavar "USERNAME"
        )
    <*> strOption
        ( long "mongoPass"
            <> help "Mongo password"
            <> metavar "PASSWORD"
        )
    <*> strOption
        ( long "mongoDb"
            <> help "Mongo database"
            <> metavar "DBNAME"
        )
    <*> switch
        ( long "mongoTls"
          <> help "TLS for mongodb"
          <> showDefault
        )
    <*> option auto
        ( long "poolSize"
            <> help "MongoDB Connection Pool Size"
            <> metavar "POOLSIZE"
            <> showDefault
            <> value 5
        )
    <*> option auto
        ( long "conTimeout"
            <> help "Connection pool idle timeout"
            <> metavar "TIMEOUT"
            <> showDefault
            <> value (300 :: NominalDiffTime)
        )
    <*> strOption
        ( long "sslCert"
            <> help "Ssl Certificate file"
            <> metavar "CERTFILE"
        )
    <*> strOption
        ( long "sslKey"
            <> help "Ssl Key file"
            <> metavar "KEYFILE"
        )

opts ∷ ParserInfo ProgramOptions
opts =
  info
    (parseArguments <**> helper)
    (fullDesc <> progDesc "Run Haskell Servant/Wai User Micro-Service")

warpSettings ∷ AppContext → Settings
warpSettings ctx = setPort (port ctx) $
  setGracefulShutdownTimeout (Just 10) $
  setInstallShutdownHandler shutdownHanlder $
  setOnExceptionResponse exceptionResponder defaultSettings
  where
    port = serverPort . options
    shutdownHanlder closeSocket = void $ installHandler sigTERM
        (CatchOnce $ do
          logError ctx Nothing "Term / Interrupt signal sent"
          closeMongoDBPool ctx
          closeSocket
        )
        (Just $ sigINT `addSignal` emptySignalSet)

startServer ∷ AppContext → IO ()
startServer ctx = do
  logInfo ctx Nothing $ "Running with options" <> showText (sanitize $ options ctx)
  logInfo ctx Nothing $ "Server starting on port " <> showText (serverPort $ options ctx)

  runTLS tlsOpts (warpSettings ctx) $
    requestIdMiddleware ctx $
    logStartMiddleware ctx $
    exceptionLogger ctx $
    gzip def $
    withReqContext (app . addReqId ctx)
  where
    addReqId aCtx rCtx = aCtx {reqContext = rCtx}
    tlsOpts = tlsSettings (sslCert $ options ctx) (sslKey $ options ctx)

main ∷ IO ()
main = do
  o <- execParser opts
  print $ "Options: " <> show o
  pool <- createMongoDBPool o >>= \pool -> checkAuth pool o >> pure pool

  logSet <- newStdoutLoggerSet defaultBufSize
  errLogSet <- newStderrLoggerSet defaultBufSize

  startServer $ AppContext pool o logSet errLogSet (ReqContext Nothing [])
