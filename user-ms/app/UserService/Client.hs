{-# LANGUAGE TypeApplications #-}

module UserService.Client
( BaseUserApiRoutes (..),
  AdminRoutes (..),
  UserApiRoutes (..),
  UserRoutes (..),
  userClient,
  adminApi,
  userApi,
)
where

import Servant                  (Proxy (Proxy))
import Servant.Auth.Client      (Token)
import Servant.Client.Streaming (AsClientT, ClientM, client, (//), (/:))
import UserService.Server       (AdminRoutes (..), BaseUserApiRoutes (..),
                                 UserApi, UserApiRoutes (..), UserRoutes (..))
{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a client library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

userClient ∷ BaseUserApiRoutes (AsClientT ClientM)
userClient = client (Proxy @UserApi)

adminApi ∷ Token → AdminRoutes (AsClientT ClientM)
adminApi token = userClient // baseUrl // adminRoutes /: token

userApi ∷ Token → UserRoutes (AsClientT ClientM)
userApi token = userClient // baseUrl // userRoutes /: token
