module UserService.Handlers
  ( delUser
  , getUser
  , saveUser
  , searchUsers
  , totalUsers
  , updateUser
  ) where
import Control.Exception       (Exception (displayException))
import Data.Aeson              (Value, object, (.=))
import Data.Bool               (bool)
import Data.Text               (Text, pack)
import MSFramework.Data        (AppM)
import MSFramework.Logger      (toJsonForLog)
import MSFramework.Servant     (ReqLogger (..), sendJSONError)
import MSFramework.Util        (showText)
import Servant                 (NoContent (..), err400, err404, throwError)
import UserService.Persistence qualified as P
import UserService.Types       (UpdateUser, User, UserSearch)

-- | Handler for the searchUsers endpoint.
searchUsers ∷ ReqLogger → UserSearch → AppM [User]
searchUsers rlog@ReqLogger{logDebug, logError} userSearch = do
    logDebug $ "getting users with query: " <> showText userSearch
    P.searchUsers rlog userSearch >>= \case
      Left e -> do
          logError $ "Failed to search users" <> pack (displayException e)
          throwError err400
      Right r -> pure $ bool r mempty (null r)

-- | Handler for the SaveUser endoint.
saveUser ∷ ReqLogger → User → AppM User
saveUser rlog@ReqLogger{logDebug, logError} user = do
    logDebug $ "Request to store user: " <> toJsonForLog user
    P.insertUser rlog user >>= \case
       Left e -> do
           logError $ "Failed to save user: " <> pack (displayException e)
           sendJSONError "Failed to save user"
       Right u -> pure u

-- | Handler for the GetUser endpoint.
getUser ∷ ReqLogger → Text → AppM User
getUser rlog@ReqLogger{logDebug, logError} key = do
    logDebug $ "Getting user with id " <> key
    P.getUser rlog key >>= \case
        Left e -> do
            logError $ "Failed to get user: " <> pack (displayException e)
            sendJSONError "Failed to get user"
        Right u -> case u of
            Just user -> pure user
            Nothing   -> throwError err404

-- | Handler for the DelUser endpoint.
delUser ∷ ReqLogger → Text → AppM NoContent
delUser rlog@ReqLogger{logError} key = P.deleteUser rlog key >>= \case
    Left e -> do
        logError $ "Failed to delete user: " <> pack (displayException e)
        sendJSONError "Failed to delete user"
    Right _ -> pure NoContent

-- | Handler for the ChangeUser endpoint.
updateUser ∷ ReqLogger → UpdateUser → AppM NoContent
updateUser rlog@ReqLogger{logError} user =
    P.updateUser rlog user >>= \case
        Left e -> do
            logError $ "Failed to update user: " <> pack (displayException e)
            sendJSONError "Failed to update user"
        Right _ -> pure NoContent

-- | Handler for the UserCount endpoint.
totalUsers ∷ ReqLogger → AppM [Value]
totalUsers rlog@ReqLogger{logError} =
    P.totalUsers rlog >>= \case
        Left e -> do
            logError $ "Failed to get total user counts: " <> pack (displayException e)
            sendJSONError "Failed to get counts"
        Right r -> pure $ fmap toObject r
    where
        toObject ∷ (Text, Int) → Value
        toObject (t, i) = object [t .= i]
