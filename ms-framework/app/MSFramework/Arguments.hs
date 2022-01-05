module MSFramework.Arguments
  ( parseMongoOptions
  , parserServerOptions
  ) where

import Database.MongoDB    (Host (..), readHostPortM)
import MSFramework.Types   (LogLevel (..), MongoOptions (..),
                            ServerOptions (..))
import Options.Applicative (Parser, ReadM, auto, eitherReader, help, long,
                            metavar, option, short, showDefault, strOption,
                            switch, value)

parseMongoHost ∷ ReadM Host
parseMongoHost = eitherReader $ \s ->
  let mHost = readHostPortM s
   in maybe (Left $ "invalid host port " <> s) Right mHost

parseLogLevel ∷ ReadM LogLevel
parseLogLevel = eitherReader $ \case
  "debug" -> Right Debug
  "info"  -> Right Info
  "error" -> Right Error
  "warn"  -> Right Warn
  s       -> Left $ "Invalid argument: " <> s

parseMongoOptions ∷ Parser MongoOptions
parseMongoOptions =
  MongoOptions
    <$> strOption
        ( long "collectionName"
            <> help "MongoDB Collection Name"
            <> metavar "NAME"
        )
    <*> option parseMongoHost
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
    <*> strOption
        ( long "mongoClientCert"
            <> help "Mongo client certificate file"
            <> metavar "CERTFILE"
        )
    <*> strOption
        ( long "mongoKeyFile"
            <> help "Mongo private key file"
            <> metavar "KEYFILE"
        )
    <*> strOption
        ( long "mongoCaFile"
            <> help "mongo CA file"
            <> metavar "CAFILE"
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
            <> value 300
        )

parserServerOptions ∷ Parser ServerOptions
parserServerOptions =
  ServerOptions
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
        ( long "sslCert"
            <> help "Ssl Certificate file"
            <> metavar "CERTFILE"
        )
    <*> strOption
        ( long "sslKey"
            <> help "Ssl Key file"
            <> metavar "KEYFILE"
        )

    <*> strOption
        ( long "hashPrefix"
            <> help "Hash prefix"
            <> metavar "HASHPREFIX"
        )
    <*> strOption
        ( long "jwkFile"
            <> help "JWK File"
            <> metavar "JWKFILE"
        )
