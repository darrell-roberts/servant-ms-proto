{-# LANGUAGE DataKinds, TypeOperators #-}

module UserService.Server
  ( UserApi
  , app
  , userApi
  ) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson           (Value)
import Data.ByteString      (ByteString)
import Data.Text            (Text)
import MSFramework.Servant  (errorFormatters)
import MSFramework.Types    (AppContext, AppM(..))
import Servant              (Application, Capture, Context (EmptyContext, (:.)),
                             Delete, Get, Handler, HasServer (ServerT), JSON,
                             NoContent, NoFraming, OctetStream, Post,
                             Proxy (..), Put, ReqBody, SourceIO, StreamGet,
                             hoistServer, serveWithContext, type (:<|>) (..),
                             type (:>))
import UserService.Handlers (delUser, downloadUsers, getUser, saveUser,
                             searchUsers, totalUsers, updateUser)
import UserService.Types    (UpdateUser, User, UserSearch)

-- Handlers.
type SearchUsers = "search" :> ReqBody '[JSON] UserSearch :> Post '[JSON] [User]
type SaveUser = ReqBody '[JSON] User :> Post '[JSON] User
type GetUser = Capture "id" Text :> Get '[JSON] User
type DelUser = Capture "id" Text :> Delete '[JSON] NoContent
type ChangeUser = ReqBody '[JSON] UpdateUser :> Put '[JSON] NoContent
type UserCount = "counts" :> Get '[JSON] [Value]
type DownLoad = "download" :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type UserApi = "api" :> "v1" :> "user" :>
  ( SearchUsers :<|>
    SaveUser :<|>
    UserCount :<|>
    DownLoad :<|>
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
             downloadUsers :<|>
             getUser :<|>
             delUser :<|>
             updateUser

-- | Run handlers within Reader Monad Transformer providing application context.
app ∷ AppContext → Application
app ctx = serve $ hoistServer userApi nt userServer
 where
  nt ∷ AppM a → Handler a
  nt (AppM reader) = runReaderT reader ctx

  serve = serveWithContext userApi (errorFormatters :. EmptyContext)
