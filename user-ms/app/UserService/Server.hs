{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications, TypeOperators #-}

module UserService.Server
  ( UserApi
  , BaseUserApiRoutes(..)
  , UserRoutes (..)
  , AdminRoutes (..)
  , UserApiRoutes (..)
  , app
  ) where

import Control.Monad.Reader   (ReaderT (runReaderT))
import Crypto.JOSE.JWK        (JWK)
import Data.Aeson             (Value (..), encode, object, (.=))
import Data.ByteString        (ByteString)
import Data.Text              (Text)
import MSFramework.Servant    (errorFormatters)
import Servant                (Application, Capture,
                               Context (EmptyContext, (:.)), Delete, Get,
                               Handler, JSON, NamedRoutes, NoContent, NoFraming,
                               OctetStream, Post, Proxy (..), Put, ReqBody,
                               ServerError (..), SourceIO, StreamGet, err403,
                               serveWithContextT, type (:>))
import Servant.API.Generic    (Generic, GenericMode (type (:-)))
import Servant.Auth.Server    (Auth, AuthResult (..), JWT,
                               defaultCookieSettings, defaultJWTSettings,
                               throwAll)
import Servant.Server.Generic (AsServerT)
import UserService.Handlers   (delUser, downloadUsers, getUser, saveUser,
                               searchUsers, totalUsers, updateUser,
                               withHashable)
import UserService.Security   (HashedUser)
import UserService.Types      (AppM, Role (..), UpdateUser, User, UserAuth (..),
                               UserMsAppContext, UserSearch)

type UserApi = NamedRoutes BaseUserApiRoutes

newtype BaseUserApiRoutes mode
  = BaseUserApiRoutes { baseUrl :: mode :- "api" :> "v1" :> "user" :> NamedRoutes UserApiRoutes }
  deriving (Generic)

data UserRoutes mode = UserRoutes
  { saveUserRoute  :: mode :- ReqBody '[JSON] User :> Post '[JSON] HashedUser
  , userCountRoute :: mode :- "counts" :> Get '[JSON] [Value]
  }
  deriving (Generic)

data AdminRoutes mode = AdminRoutes
  { searchUsersRoute :: mode :- "search" :> ReqBody '[JSON] UserSearch :> Post '[JSON] [HashedUser]
  , downloadUsersRoute :: mode :- "download" :> StreamGet NoFraming OctetStream (SourceIO ByteString)
  , getUserRoute :: mode :- Capture "id" Text :> Get '[JSON] HashedUser
  , delUserRoute :: mode :- Capture "id" Text :> Delete '[JSON] NoContent
  , changeUserRoute :: mode :- ReqBody '[JSON] UpdateUser :> Put '[JSON] NoContent
  }
  deriving (Generic)

data UserApiRoutes mode = UserApiRoutes
  { userRoutes :: mode :- Auth '[JWT] (UserAuth 'NormalUser) :> NamedRoutes UserRoutes
  , adminRoutes :: mode :- Auth '[JWT] (UserAuth 'Admin) :> NamedRoutes AdminRoutes
  }
  deriving (Generic)

accessDenied ∷ Text → ServerError
accessDenied routes = err403
  { errBody = encode $ object [ "message" .= String ("access denied for " <> routes) ]
  , errHeaders = [("Content-Type", "application/json")]
  }

userServer ∷ BaseUserApiRoutes (AsServerT AppM)
userServer = BaseUserApiRoutes UserApiRoutes
  { adminRoutes = \case
      Authenticated userAuth -> AdminRoutes
        { searchUsersRoute = withHashable . searchUsers userAuth
        , downloadUsersRoute = downloadUsers
        , getUserRoute = withHashable . getUser
        , delUserRoute = delUser
        , changeUserRoute = updateUser
        }
      _ -> throwAll $ accessDenied "adminRoutes"
  , userRoutes = \case
      Authenticated userAuth -> UserRoutes
        { saveUserRoute = withHashable . saveUser
        , userCountRoute = totalUsers userAuth
        }
      _ -> throwAll $ accessDenied "userRoutes"
  }

-- | Run handlers within Reader Monad Transformer providing application context.
app ∷ JWK → UserMsAppContext → Application
app key ctx = serveWithContextT (Proxy @UserApi) serverContext nt userServer
 where
  nt ∷ AppM a → Handler a
  nt reader = runReaderT reader ctx

  serverContext = errorFormatters :. jwtCfg :. defaultCookieSettings :. EmptyContext
  jwtCfg = defaultJWTSettings key
