{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ScopedTypeVariables #-}

module UserService.Types
  ( Gender (..)
  , LogLevel (..)
  , UpdateUser (..)
  , User (..)
  , UserSearch (..)
  , fromBson
  , toBson
  ) where

import Data.Aeson       (FromJSON (..), ToJSON (..), object, withObject, (.:),
                         (.:?), (.=))
import Data.Bson        (Document, ObjectId, Val, Value (..), cast', lookup,
                         val, (=:))
import Data.Text        (Text)
import GHC.Generics     (Generic)
import MSFramework.Data (LogLevel (..), Sanitize, sanitize)
import MSFramework.Util (maskShowFirstLast, showText)
import Prelude          hiding (id, lookup)
import Text.Read        (readMaybe)

data Gender = Male | Female deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- | Bson conversion for custom type.
instance Val Gender where
    val g = case g of
        Male   -> String "Male"
        Female -> String "Female"
    cast' g = case g of
        String "Male"   -> Just Male
        String "Female" -> Just Female
        _               -> Nothing

-- | User data type.
data User
   = User
     { name   :: !Text
     , age    :: !Int
     , email  :: !Text
     , gender :: !Gender
     , id     :: !(Maybe Text)
     }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

data UpdateUser
   = UpdateUser
     { id    :: !ObjectId
     , name  :: !(Maybe Text)
     , email :: !(Maybe Text)
     }
  deriving (Eq, Generic, Show)

instance FromJSON UpdateUser where
    parseJSON = withObject "UpdateUser" $ \v -> do
        key <- v .: "id"
        name <- v .:? "name"
        email <- v.:? "email"

        case readMaybe key of
            Just id -> pure $ UpdateUser {name, email, id}
            Nothing -> fail $ "invalid id " <> key

instance ToJSON UpdateUser where
  toJSON UpdateUser{id, name, email} =
    object ["id" .= show id, "name" .= name, "email" .= email]

data UserSearch
   = UserSearch
     { email  :: !(Maybe Text)
     , gender :: !(Maybe Gender)
     , name   :: !(Maybe Text)
     }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- BSON conversions for MongoDB.
class ToBson a where
    toBson :: a -> Document

class FromBson a where
    fromBson :: MonadFail m => Document -> m a

instance ToBson User where
    toBson User{name, age, email, gender} =
        [ "name" =: name
        , "age" =: age
        , "email" =: email
        , "gender" =: gender
        ]

instance FromBson User where
    fromBson doc = do
        name <- "name" `lookup` doc
        age <- "age" `lookup` doc
        email <- "email" `lookup` doc
        gender <- "gender" `lookup` doc
        (i :: ObjectId) <- "_id" `lookup` doc
        let id = Just $ showText i
        pure User{name, age, email, gender, id}

instance ToBson UpdateUser where
    toBson UpdateUser{id, name, email} =
        [ "name" =: name
        , "_id" =: id
        , "email" =: email
        ]

instance Sanitize User where
    sanitize u@User{name, email} =
      u { name = maskShowFirstLast name
        , email = maskShowFirstLast email
        }

instance Sanitize UpdateUser where
    sanitize u@UpdateUser{name, email} =
      u { name = fmap maskShowFirstLast name
        , email = fmap maskShowFirstLast email
        }
