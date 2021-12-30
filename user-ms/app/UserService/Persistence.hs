{-# LANGUAGE ExtendedDefaultRules #-}

module UserService.Persistence
  ( deleteUser
  , getUser
  , insertUser
  , searchUsers
  , totalUsers
  , updateUser
  , sourceAllUsers
  ) where

import Conduit                    (ConduitT, MonadIO, (.|))
import Control.Monad              ((>=>))
import Control.Monad.Reader.Class (MonadReader)
import Data.Conduit.Combinators   qualified as C
import Data.Maybe                 (mapMaybe)
import Data.Text                  (Text)
import Database.MongoDB           (Action, Document, Failure, Query (..),
                                   Select (select), Value (..), aggregate,
                                   deleteOne, find, findOne, insert, lookup,
                                   modify, rest, (=:))
import Database.MongoDB.Query     (Collection)
import MSFramework.Logger         (logDebug)
import MSFramework.MongoUtil      (SanitizedQuery (..), asMongoKey,
                                   filterBsonId, filterBsonNull, runAction,
                                   sourceQuery)
import MSFramework.Types          (AppContext (..), Sanitize (sanitize))
import MSFramework.Util           (showText)
import Prelude                    hiding (id, lookup)
import UserService.Types          (UpdateUser (..), User (..), UserSearch (..),
                                   fromBson, toBson)

-- | Mongodb Collection name.
usersCollection ∷ Collection
usersCollection = "users"

-- | Build mongo query from optional search fields.
getUserSearchQuery ∷ UserSearch → Query
getUserSearchQuery UserSearch{email, gender, name} =
  select
    (filterBsonNull ["email" =: email, "gender" =: gender, "name" =: name])
    usersCollection

-- | Lookup all the users in the mongo users collection.
searchUsers ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  UserSearch →
  m (Either Failure [User])
searchUsers userSearch = do
  let searchQuery = getUserSearchQuery userSearch
  logDebug $ showText (sanitize $ SanitizedQuery searchQuery)
  runAction $ search searchQuery
  where
    search ∷ (MonadIO m, MonadFail m) ⇒ Query → Action m [User]
    search = find >=> rest >=> mapM fromBson

-- | Lookup a single user by primary key.
getUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  Text →
  m (Either Failure (Maybe User))
getUser key =
  case asMongoKey key of
    Just k  -> runAction (findOne (select ["_id" =: k] usersCollection)
      >>= mapM fromBson)
    Nothing -> pure $ Right Nothing

-- | Insert a user into the mongo users collection.
insertUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  User →
  m (Either Failure User)
insertUser user =
  fmap (\case
          -- add key to original document
          ObjId k -> user{id = Just $ showText k} :: User
          _       -> user
        ) <$> runAction (insert usersCollection $ toBson user)

-- | Delete a user from the mongo users collection.
deleteUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  Text →
  m (Either Failure ())
deleteUser key =
  case asMongoKey key of
    Just k  -> runAction $ deleteOne (select ["_id" =: k] usersCollection)
    Nothing -> pure $ Right ()

-- | Update a user with optional fields.
updateUser ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  UpdateUser →
  m (Either Failure ())
updateUser u@UpdateUser{id} = do
  let updatedDoc = updateFields u
  logDebug $ "Updating user with: "
    <> showText (sanitize updatedDoc)
    <> " using id "
    <> showText id
  runAction $ modify (select ["_id" =: id] usersCollection)
    ["$set" =: updatedDoc]
  where
    updateFields = filterBsonId . filterBsonNull . toBson

-- | Group users by gender and get total counts.
totalUsers ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  m (Either Failure [(Text, Int)])
totalUsers = do
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

-- | A conduit producer that produces all user documents.
sourceAllUsers ∷ AppContext → ConduitT () User IO ()
sourceAllUsers ctx = sourceQuery ctx (select [] usersCollection) .| C.mapM fromBson
