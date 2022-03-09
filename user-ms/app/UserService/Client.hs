{-# LANGUAGE DataKinds, ExplicitNamespaces, ScopedTypeVariables,
             TypeApplications #-}

module UserService.Client
( BaseUserApiRoutes (..),
  AdminRoutes (..),
  UserApiRoutes (..),
  apiClient
)
where


import Servant                  (NamedRoutes, Proxy (Proxy))
import Servant.Auth.Client      (Token)
import Servant.Client.Streaming (AsClientT, ClientM, client)
import UserService.Server       (AdminRoutes (..), BaseUserApiRoutes (..),
                                 UserApiRoutes (..))
{-
  https://docs.servant.dev/en/stable/tutorial/Client.html

  Since I have the server and types as modules in user-ms I can build a client library
  as well in user-ms that exposes these two modules allowing me to import them
  here in a client app (or another micro-service) and use servant-client to
  infer the client functions. This keeps the types and api specifications in sync and
  would be validated at compile time.
-}

-- searchUsers ∷ Token → UserSearch → ClientM [HashedUser]
-- saveUser ∷ Token → User → ClientM HashedUser
-- getUser ∷ Token → Text → ClientM HashedUser
-- delUser ∷ Token → Text → ClientM NoContent
-- changeUser ∷ Token → UpdateUser → ClientM NoContent
-- userCount ∷ Token → ClientM [Value]
-- downloadUser ∷ Token → ClientM (SourceIO ByteString)

-- saveUser
--    :<|> userCount
--    :<|> searchUsers
--    :<|> downloadUser
--    :<|> getUser
--    :<|> delUser
--    :<|> changeUser
--    = client $ flatten userApi

apiClient ∷ BaseUserApiRoutes (AsClientT ClientM)
apiClient = client (Proxy @(NamedRoutes BaseUserApiRoutes))
