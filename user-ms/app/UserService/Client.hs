{-# LANGUAGE DataKinds, ExplicitNamespaces, ScopedTypeVariables #-}

module UserService.Client
  ( changeUser
  , delUser
  , getUser
  , saveUser
  , searchUsers
  , userCount
  ) where

import Data.Aeson         (Value)
import Data.Text          (Text)
import Servant.API        (NoContent, type (:<|>) ((:<|>)))
import Servant.Client     (ClientM, client)
import UserService.Server (userApi)
import UserService.Types  (UpdateUser, User, UserSearch)

{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a client library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

searchUsers ∷ UserSearch → ClientM [User]
saveUser ∷ User → ClientM User
getUser ∷ ClientM [Value]
delUser ∷ Text → ClientM User
changeUser ∷ Text → ClientM NoContent
userCount ∷ UpdateUser → ClientM NoContent

searchUsers :<|> saveUser :<|> getUser :<|> delUser :<|> changeUser :<|> userCount =
  client userApi

