{-# LANGUAGE DataKinds, TypeOperators #-}

module UserService.Server
  ( UserApi
  , UserAccessApi
  , AdminAccessApi
  , app
  , userApi
  ) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Crypto.JOSE.JWK      (JWK)
import Data.Aeson           (Value (..), encode, object, (.=))
import Data.ByteString      (ByteString)
import Data.Text            (Text)
import MSFramework.Servant  (errorFormatters)
import Servant              (Application, Capture, Context (EmptyContext, (:.)),
                             Delete, Get, Handler, HasServer (ServerT), JSON,
                             NoContent, NoFraming, OctetStream, Post, layoutWithContext,
                             Proxy (..), Put, ReqBody, ServerError (..),
                             SourceIO, StreamGet, err403,
                             hoistServerWithContext, serveWithContext,
                             type (:<|>) (..), type (:>))
import Servant.Auth.Server  (Auth, AuthResult (..), CookieSettings, JWT,
                             JWTSettings, defaultCookieSettings,
                             defaultJWTSettings, throwAll)
import UserService.Handlers (delUser, downloadUsers, getUser, saveUser,
                             searchUsers, totalUsers, updateUser, withHashable)
import UserService.Security (HashedUser)
import UserService.Types    (AppM, Role (..), UpdateUser, User, UserClaims (..),
                             UserMsAppContext, UserSearch)

-- API.
type SearchUsers = "search" :> ReqBody '[JSON] UserSearch :> Post '[JSON] [HashedUser]
type SaveUser = ReqBody '[JSON] User :> Post '[JSON] HashedUser
type GetUser = Capture "id" Text :> Get '[JSON] HashedUser
type DelUser = Capture "id" Text :> Delete '[JSON] NoContent
type ChangeUser = ReqBody '[JSON] UpdateUser :> Put '[JSON] NoContent
type UserCount = "counts" :> Get '[JSON] [Value]
type DownLoad = "download" :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type UserAccessApi = Auth '[JWT] (UserClaims 'NormalUser) :> (SaveUser :<|> UserCount)

type AdminAccessApi = Auth '[JWT] (UserClaims 'Admin) :>
  ( SearchUsers :<|>
    DownLoad :<|>
    GetUser :<|>
    DelUser :<|>
    ChangeUser
  )

type UserApi = "api" :> "v1" :> "user" :> (UserAccessApi :<|> AdminAccessApi)

userApi ∷ Proxy UserApi
userApi = Proxy

accessDenied ∷ ServerError
accessDenied = err403
      { errBody = encode $ object [ "message" .= String "access denied" ]
      , errHeaders = [("Content-Type", "application/json")]
      }

-- | API's to handlers
userServer ∷ ServerT UserApi AppM
userServer = userAccessApi :<|> adminAccessApi
  where
    userAccessApi (Authenticated _claims) =
      withHashable . saveUser :<|> totalUsers
    userAccessApi _                       = throwAll accessDenied

    adminAccessApi (Authenticated _claims) =
      withHashable . searchUsers :<|>
      downloadUsers  :<|>
      withHashable . getUser :<|>
      delUser :<|>
      updateUser
    adminAccessApi _                       = throwAll accessDenied

-- | Run handlers within Reader Monad Transformer providing application context.
app ∷ JWK → UserMsAppContext → Application
app key ctx = serve $
  hoistServerWithContext
    userApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) nt userServer
 where
  nt ∷ AppM a → Handler a
  nt reader = runReaderT reader ctx

  serverContext = errorFormatters :. jwtCfg :. defaultCookieSettings :. EmptyContext
  jwtCfg = defaultJWTSettings key
  serve = serveWithContext userApi serverContext
