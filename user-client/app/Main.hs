{-# LANGUAGE ExplicitNamespaces #-}

module Main where

import Data.Aeson               (Value)
import Data.ByteString.Lazy     (toStrict)
import Data.Text                (Text)
import Network.Connection       (TLSSettings (..))
import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS  (mkManagerSettings)
import Options.Applicative      (Parser, ParserInfo, ReadM, auto, eitherReader,
                                 execParser, fullDesc, help, helper, info, long,
                                 metavar, option, progDesc, short, showDefault,
                                 strOption, switch, value, (<**>))
import Servant.API              (NoContent, type (:<|>) ((:<|>)))
import Servant.Auth.Client      (Token (..))
import Servant.Client.Streaming (BaseUrl (BaseUrl), ClientM,
                                 Scheme (Http, Https), client, mkClientEnv,
                                 withClientM, (//), (/:))
import System.IO                (hPutStrLn, stderr)
import UserService.Client       (AdminRoutes (..), BaseUserApiRoutes (..),
                                 UserApiRoutes (..), apiClient)
import UserService.Security     (HashedUser, createJwt, getJWK)
import UserService.Types        (Email (..), Gender (..), Role (Admin),
                                 UpdateUser (..), User, UserSearch (UserSearch))
{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

data ProgramOpts = ProgramOpts
  { jwkFile    :: !FilePath
  , serverHost :: !String
  , serverPort :: !Int
  }

parseProgramOpts ∷ Parser ProgramOpts
parseProgramOpts = ProgramOpts
  <$> strOption
      ( long "jwkFile"
          <> help "JWK File"
          <> metavar "JWKFILE"
      )
  <*> strOption
      ( long "serverHost"
          <> help "Server Host"
          <> metavar "HOSTNAME"
      )
  <*> option auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port number for API server"
      )

programOpts ∷ ParserInfo ProgramOpts
programOpts = info (parseProgramOpts <**> helper)
  (fullDesc <> progDesc "Run Haskell Servant client")

queryUsers ∷ Token → ClientM [HashedUser]
queryUsers token =
  apiClient //
  baseUrl //
  adminRoutes /: token //
  searchUsersRoute /:
  UserSearch Nothing (Just Male) Nothing

main ∷ IO ()
main = do
  ProgramOpts {jwkFile, serverHost, serverPort} <- execParser programOpts

  jwk <- getJWK jwkFile
  userJwt <- fmap (Token . toStrict) <$> createJwt jwk Admin

  case userJwt of
    Left err  -> hPutStrLn stderr ("Failed to create jwt" <> show err)
    Right token -> do
      manager <- newManager tlsSelfSigned
      withClientM
        (queryUsers token)
        (mkClientEnv manager $ BaseUrl Https serverHost serverPort "") $
          \case
            Left err    -> putStrLn $ "Error: " <> show err
            Right users -> print users

  where
    tlsSelfSigned = mkManagerSettings
      TLSSettingsSimple
      { settingDisableCertificateValidation = True
      , settingDisableSession = True
      , settingUseServerName = False
      }
      Nothing
