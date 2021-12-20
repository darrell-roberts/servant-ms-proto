module Main where

import Database.MongoDB            (Host (..), Pipe, access, auth, connect,
                                    defaultPort, master)
import MSFramework.Data            (AppContext (..), LogLevel (..),
                                    ProgramOptions (..), Sanitize (sanitize))
import MSFramework.Middleware      (exceptionLogger, exceptionResponder,
                                    logInfo, logStartMiddleware,
                                    requestIdMiddleware)
import MSFramework.Util            (showText)
import Network.Wai.Handler.Warp    (Settings, defaultSettings, runSettings,
                                    setOnException, setOnExceptionResponse,
                                    setPort)
import Network.Wai.Middleware.Gzip (def, gzip)
import Options.Applicative
import System.IO                   (hPutStrLn, stderr)
import System.Log.FastLogger       (defaultBufSize, newStderrLoggerSet,
                                    newStdoutLoggerSet)
import UserService.Server          (app)

parseMongoOptions ∷ ReadM Host
parseMongoOptions = eitherReader $ \s ->
    Right $ Host s defaultPort

parseLogLevel ∷ ReadM LogLevel
parseLogLevel = eitherReader $ \case
    "debug" -> Right Debug
    "info"  -> Right Info
    "error" -> Right Error
    "warn"  -> Right Warn
    s       -> Left $ "Invalid argument: " <> s

parseArguments ∷ Parser ProgramOptions
parseArguments =
    ProgramOptions
        <$> option
            parseMongoOptions
            ( long "mongoHost"
                <> help "MongoDB Host Name"
                <> metavar "HOST"
            )
        <*> option auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Port number for API server"
            )
        <*> strOption
            ( long "collectionName"
                <> help "MongoDB Collection Name"
                <> metavar "NAME"
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

opts ∷ ParserInfo ProgramOptions
opts =
    info
        (parseArguments <**> helper)
        (fullDesc <> progDesc "Run Haskell Servant Micro-Service")

warpSettings ∷ AppContext → Settings
warpSettings ctx = setPort (port ctx) $
    setOnException (\_ e -> putStrLn $ "Caught exception " <> show e) $
    setOnExceptionResponse exceptionResponder
    defaultSettings
    where
        port AppContext{options} = serverPort options

startServer ∷ Pipe → ProgramOptions → IO ()
startServer pipe options = do
    logSet <- newStdoutLoggerSet defaultBufSize
    errLogSet <- newStderrLoggerSet defaultBufSize

    let ctx = AppContext pipe options logSet errLogSet

    logInfo ctx Nothing $ "Running with options" <> showText (sanitize options)
    logInfo ctx Nothing $ "Server starting on port " <> showText (serverPort options)

    runSettings (warpSettings ctx) $
        gzip def $
        requestIdMiddleware ctx $
        logStartMiddleware ctx $
        exceptionLogger ctx $
        app ctx

main ∷ IO ()
main = do
    o@ProgramOptions{mongoHost, mongoUser, mongoPass, mongoDb} <- execParser opts
    pipe <- connect mongoHost
    authResult <- access pipe master mongoDb $ auth mongoUser mongoPass
    if authResult
        then startServer pipe o
        else hPutStrLn stderr "Failed to authenticate with mongodb"
