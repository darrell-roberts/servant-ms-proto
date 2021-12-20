{-# LANGUAGE ExtendedDefaultRules #-}

module UserService.Persistence
  ( deleteUser
  , getUser
  , insertUser
  , searchUsers
  , totalUsers
  , updateUser
  ) where

import Control.Monad              ((>=>))
import Control.Monad.Reader.Class (MonadReader)
import Data.Maybe                 (mapMaybe)
import Data.Text                  (Text)
import Database.MongoDB           (Action, Document, Failure, Query (..),
                                   Select (select), Value (..), aggregate,
                                   deleteOne, find, findOne, insert, lookup,
                                   modify, rest, (=:))
import Database.MongoDB.Query     (Collection)
import MSFramework.Data           (AppContext (..))
import MSFramework.MongoUtil      (asMongoKey, filterBsonId, filterBsonNull,
                                   runAction)
import MSFramework.Servant        (ReqLogger (..))
import MSFramework.Util           (showText)
import Prelude                    hiding (id, lookup)
import UnliftIO                   (MonadIO)
import UserService.Types          (UpdateUser (..), User (..), UserSearch (..),
                                   fromBson, toBson)

-- | Mongodb Collection name.
usersCollection ∷ Collection
usersCollection = "users"

-- | Build mongo query from optional search fields.
getUserSearchQuery ∷ UserSearch → Query
getUserSearchQuery UserSearch{email, gender, name} =
  select (filterBsonNull ["email" =: email, "gender" =: gender, "name" =: name])
    usersCollection

-- | Lookup all the users in the mongo users collection.
searchUsers ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  UserSearch →
  m (Either Failure [User])
searchUsers ReqLogger{logDebug} userSearch = do
  let searchQuery = getUserSearchQuery userSearch
  logDebug $ showText searchQuery
  runAction $ search searchQuery
  where
    search ∷ ∀ m. (MonadIO m, MonadFail m) ⇒ Query → Action m [User]
    search = find >=> rest >=> mapM fromBson

-- | Lookup a single user by primary key.
getUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  Text →
  m (Either Failure (Maybe User))
getUser _ key =
  case asMongoKey key of
    Just k  -> runAction (findOne (select ["_id" =: k] usersCollection)
      >>= mapM fromBson)
    Nothing -> pure $ Right Nothing

-- | Insert a user into the mongo users collection.
insertUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  User →
  m (Either Failure User)
insertUser _ user =
  fmap (\case
          -- add key to original document
          ObjId k -> user{id = Just $ showText k} :: User
          _       -> user
        ) <$> runAction (insert usersCollection $ toBson user)

-- | Delete a user from the mongo users collection.
deleteUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  Text →
  m (Either Failure ())
deleteUser _ key =
  case asMongoKey key of
    Just k  -> runAction $ deleteOne (select ["_id" =: k] usersCollection)
    Nothing -> pure $ Right ()

-- | Update a user with optional fields.
updateUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  UpdateUser →
  m (Either Failure ())
updateUser ReqLogger{logDebug} u@UpdateUser{id} = do
  let updatedDoc = updateFields u
  logDebug $ "Updating user with: " <> showText updatedDoc <> " using id " <> showText id
  runAction $ modify (select ["_id" =: id] usersCollection) ["$set" =: updatedDoc]
  where
    updateFields = filterBsonId . filterBsonNull . toBson

-- | Group users by gender and get total counts.
totalUsers ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  ReqLogger →
  m (Either Failure [(Text, Int)])
totalUsers ReqLogger{logDebug} = do
  result <- runAction $ aggregate usersCollection
    [ ["$group" =: [ "_id" =: String "$gender"
                   , "count" =: [ "$count" =: Doc [] ]
                   ]
      ]
    ]
  logDebug $ "Aggreation result: " <> showText result
  pure $ fmap getResults result
  where
    getResults ∷ [Document] → [(Text, Int)]
    getResults = mapMaybe $ \doc -> do
      gender <- "_id" `lookup` doc :: Maybe Text
      count <- "count" `lookup` doc :: Maybe Int
      pure (gender, count)