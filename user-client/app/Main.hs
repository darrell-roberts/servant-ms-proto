{-# LANGUAGE ExplicitNamespaces, ScopedTypeVariables #-}

module Main where

import Data.Aeson          (Value)
import Data.Text           (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API         (NoContent, type (:<|>) ((:<|>)))
import Servant.Client      (BaseUrl (BaseUrl), ClientM, Scheme (Http), client,
                            mkClientEnv, runClientM)
import UserService.Client  (searchUsers)
import UserService.Types   (UpdateUser (..), User, UserSearch (UserSearch))

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
  searchUsers $ UserSearch (Just "droberts@nowhere.com") Nothing Nothing

main ∷ IO ()
main = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queryUsers (mkClientEnv manager $ BaseUrl Http "localhost" 8081 "")
  case res of
    Left err                -> putStrLn $ "Error " <> show err
    Right (users :: [User]) -> print users
