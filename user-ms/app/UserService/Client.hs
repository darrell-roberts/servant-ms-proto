{-# LANGUAGE DataKinds, ExplicitNamespaces, ScopedTypeVariables #-}

module UserService.Client
  ( changeUser
  , delUser
  , getUser
  , saveUser
  , searchUsers
  , userCount
  , downloadUser
  ) where

import Data.Aeson               (Value)
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Servant.API              (NoContent, SourceIO, type (:<|>) ((:<|>)))
import Servant.Client.Streaming qualified as S
import UserService.Server       (userApi)
import UserService.Types        (UpdateUser, User, UserSearch)

{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a client library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

searchUsers ∷ UserSearch → S.ClientM [User]
saveUser ∷ User → S.ClientM User
getUser ∷ Text → S.ClientM User
delUser ∷ Text → S.ClientM NoContent
changeUser ∷ UpdateUser → S.ClientM NoContent
userCount ∷ S.ClientM [Value]
downloadUser ∷ S.ClientM (SourceIO ByteString)

searchUsers
  :<|> saveUser
  :<|> userCount
  :<|> downloadUser
  :<|> getUser
  :<|> delUser
  :<|> changeUser = S.client userApi

