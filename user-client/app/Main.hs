{-# LANGUAGE ExplicitNamespaces #-}

module Main where

import Data.Aeson               (Value)
import Data.Text                (Text)
import Network.Connection       (TLSSettings (..))
import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS  (mkManagerSettings)
import Servant.API              (NoContent, type (:<|>) ((:<|>)))
import Servant.Client.Streaming (BaseUrl (BaseUrl), ClientM,
                                 Scheme (Http, Https), client, mkClientEnv,
                                 withClientM)
import UserService.Client       (searchUsers)
import UserService.Types        (Email (..), UpdateUser (..), User,
                                 UserSearch (UserSearch))

{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

queryUsers ∷ ClientM [User]
queryUsers =
  searchUsers $ UserSearch (Just $ Email "droberts@nowhere.com") Nothing Nothing

main ∷ IO ()
main = do
  manager <- newManager tlsSelfSigned
  withClientM queryUsers (mkClientEnv manager $ BaseUrl Https "localhost" 8443 "") $ \case
    Left err    -> putStrLn $ "Error " <> show err
    Right users -> print users
  where
    tlsSelfSigned = mkManagerSettings
      TLSSettingsSimple
      { settingDisableCertificateValidation = True
      , settingDisableSession = True
      , settingUseServerName = False
      }
      Nothing
