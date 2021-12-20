{-# LANGUAGE DataKinds, TypeOperators #-}

module UserService.Server
  ( UserApi
  , app
  , userApi
  ) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson           (Value)
import Data.Text            (Text)
import MSFramework.Data     (AppContext, AppM)
import MSFramework.Servant  (ReqLogger (..), errorFormatters)
import Servant              (Application, Capture, Context (EmptyContext, (:.)),
                             Delete, Get, Handler, HasServer (ServerT), JSON,
                             NoContent, Post, Proxy (..), Put, ReqBody,
                             hoistServer, serveWithContext, type (:<|>) (..),
                             type (:>))
import UserService.Handlers (delUser, getUser, saveUser, searchUsers,
                             totalUsers, updateUser)
import UserService.Types    (UpdateUser, User, UserSearch)

-- Handlers.
type SearchUsers = "search" :> ReqLogger :> ReqBody '[JSON] UserSearch :> Post '[JSON] [User]
type SaveUser = ReqLogger :> ReqBody '[JSON] User :> Post '[JSON] User
type GetUser = ReqLogger :> Capture "id" Text :> Get '[JSON] User
type DelUser = ReqLogger :> Capture "id" Text :> Delete '[JSON] NoContent
type ChangeUser = ReqLogger :> ReqBody '[JSON] UpdateUser :> Put '[JSON] NoContent
type UserCount = "counts" :> ReqLogger :> Get '[JSON] [Value]

type UserApi = "api" :> "v1" :> "user" :>
  ( SearchUsers :<|>
    SaveUser :<|>
    UserCount :<|>
    GetUser :<|>
    DelUser :<|>
    ChangeUser
  )

userApi ∷ Proxy UserApi
userApi = Proxy

-- | Definition of endpoints that make up a user service.
userServer ∷ ServerT UserApi AppM
userServer = searchUsers :<|>
             saveUser :<|>
             totalUsers :<|>
             getUser :<|>
             delUser :<|>
             updateUser

-- | Run handlers within Reader Monad Transformer providing application context.
app ∷ AppContext → Application
app ctx = serveWithContext userApi (errorFormatters :. EmptyContext) $
  hoistServer userApi (nt ctx) userServer
 where
  nt ∷ AppContext → AppM a → Handler a
  nt e x = runReaderT x e
