{-# LANGUAGE DataKinds #-}

module UserService.Handlers
  ( delUser
  , getUser
  , saveUser
  , searchUsers
  , totalUsers
  , updateUser
  , downloadUsers
  , withHashable
  ) where

import Conduit                    ((.|))
import Control.Monad.Reader.Class (ask, reader)
import Data.Aeson                 (Value, encode, object, (.=))
import Data.Aeson.Key             (fromText)
import Data.Bool                  (bool)
import Data.ByteString            (ByteString)
import Data.ByteString.Lazy       qualified as BSL
import Data.Conduit.Combinators   qualified as C
import Data.Conduit.Zlib          (gzip)
import Data.Text                  (Text, pack)
import MSFramework.Logger         (logDebug, logError, toJsonSanitized)
import MSFramework.Servant        (sendJSONError)
import MSFramework.Types          (HasServerOptions (..), Sanitize (sanitize),
                                   ServerOptions (..))
import MSFramework.Util           (showText)
import Servant                    (NoContent (..), SourceIO, err400, err404,
                                   throwError)
import Servant.Conduit            (ConduitToSourceIO (conduitToSourceIO))
import UnliftIO                   (Exception (displayException))
import UserService.Persistence    qualified as P
import UserService.Security       (Hashable, Hashed, hash)
import UserService.Types          (AppM, Role (..), UpdateUser, User,
                                   UserAuth (..), UserSearch)

-- | Run a Hashable handler
withHashable ∷ Hashable a ⇒ AppM a → AppM (Hashed a)
withHashable handler =
  reader (hashPrefix . serverOptions) >>= \prefix -> hash prefix <$> handler

-- | Handler for the searchUsers endpoint.
searchUsers ∷ UserAuth 'Admin → UserSearch → AppM [User]
searchUsers userAuth userSearch = do
  logDebug $ "userAuth " <> showText userAuth
  logDebug $ "getting users with query: " <> showText (sanitize userSearch)
  P.searchUsers userSearch >>= \case
    Left e -> do
      logError $ "Failed to search users" <> pack (displayException e)
      throwError err400
    Right r -> pure $ bool r mempty (null r)

-- | Handler for the SaveUser endoint.
saveUser ∷ User → AppM User
saveUser user = do
  logDebug $ "Request to store user: " <> toJsonSanitized user
  P.insertUser user >>= \case
    Left e -> do
      logError $ "Failed to save user: " <> pack (displayException e)
      sendJSONError "Failed to save user"
    Right u -> pure u

-- | Handler for the GetUser endpoint.
getUser ∷ Text → AppM User
getUser key = do
  logDebug $ "Getting user with id " <> key
  P.getUser key >>= \case
    Left e -> do
      logError $ "Failed to get user: " <> pack (displayException e)
      sendJSONError "Failed to get user"
    Right u -> case u of
      Just user -> pure user
      Nothing   -> throwError err404

-- | Handler for the DelUser endpoint.
delUser ∷ Text → AppM NoContent
delUser key = P.deleteUser key >>= \case
  Left e -> do
    logError $ "Failed to delete user: " <> pack (displayException e)
    sendJSONError "Failed to delete user"
  Right _ -> pure NoContent

-- | Handler for the ChangeUser endpoint.
updateUser ∷ UpdateUser → AppM NoContent
updateUser user =
  P.updateUser user >>= \case
    Left e -> do
      logError $ "Failed to update user: " <> pack (displayException e)
      sendJSONError "Failed to update user"
    Right _ -> pure NoContent

-- | Handler for the UserCount endpoint.
totalUsers ∷ UserAuth 'NormalUser → AppM [Value]
totalUsers userAuth = do
  logDebug $ "userAuth: " <> showText userAuth
  P.totalUsers >>= \case
    Left e -> do
      logError $ "Failed to get total user counts: " <> pack (displayException e)
      sendJSONError "Failed to get counts"
    Right r -> pure $ fmap toObject r
  where
    toObject ∷ (Text, Int) → Value
    toObject (t, i) = object [fromText t .= i]

-- | Streaming handler using conduit.
downloadUsers ∷ AppM (SourceIO ByteString)
downloadUsers = conduitToSourceIO . stream <$> ask
  where
    stream ctx = P.sourceAllUsers ctx
            .| C.map (BSL.toStrict . encode)
            .| gzip
