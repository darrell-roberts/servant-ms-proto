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
import Servant.API              (NoContent, SourceIO, type (:<|>) (..))
import Servant.API.Flatten      (flatten)
import Servant.Auth.Client      (Token)
import Servant.Client.Streaming (ClientM, client)
import UserService.Security     (HashedUser)
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

searchUsers ∷ Token → UserSearch → ClientM [HashedUser]
saveUser ∷ Token → User → ClientM HashedUser
getUser ∷ Token → Text → ClientM HashedUser
delUser ∷ Token → Text → ClientM NoContent
changeUser ∷ Token → UpdateUser → ClientM NoContent
userCount ∷ Token → ClientM [Value]
downloadUser ∷ Token → ClientM (SourceIO ByteString)

saveUser
   :<|> userCount
   :<|> searchUsers
   :<|> downloadUser
   :<|> getUser
   :<|> delUser
   :<|> changeUser
   = client $ flatten userApi
