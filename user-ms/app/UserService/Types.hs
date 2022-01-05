{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleInstances,
             ScopedTypeVariables, TypeFamilies #-}

module UserService.Types (
    AppM
  , Email (..)
  , Gender (..)
  , LogLevel (..)
  , UpdateUser (..)
  , User (..)
  , UserSearch (..)
  , UserMsAppContext (..)
  , UserAppOptions (..)
  , UserClaims (..)
  , Role (..)
  , fromBson
  , toBson
  ) where

import Control.Monad         (guard)
import Control.Monad.Reader  (ReaderT)
import Data.Aeson            (FromJSON (..), ToJSON (..), object, withObject,
                              (.:), (.:?), (.=))
import Data.Bson             (Document, ObjectId, Val, Value (..), cast',
                              lookup, val, (=:))
import Data.List.NonEmpty    (NonEmpty, toList)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import MSFramework.Types     (ConnectionPool, HasLogSettings (..),
                              HasMongoConnectionPool (..), HasMongoOptions (..),
                              HasRequestContext (..), HasServerOptions (..),
                              LogLevel (..), LogSettings, MongoOptions,
                              ReqContext, Sanitize, ServerOptions, sanitize)
import MSFramework.Util      (maskShowFirstLast, showText)
import MSFramework.Validator (Validator (..))
import Prelude               hiding (id, lookup)
import Servant               (Handler)
import Servant.Auth.Server   (FromJWT, ToJWT)
import Text.Read             (readMaybe)
import Text.Regex.TDFA       ((=~))
import Validation            (Validation (..), failureIf, failureUnless)

type AppM = ReaderT UserMsAppContext Handler

-- | Application context.
data UserMsAppContext = UserMsAppContext
  { _mongoPool   :: !ConnectionPool
  , options      :: !UserAppOptions
  , _logSettings :: !LogSettings
  , reqContext   :: !ReqContext
  }

-- | Program options parsed by optparse-applicative.
data UserAppOptions = UserAppOptions
  { _serverOptions :: !ServerOptions
  , _mongoOptions  :: !MongoOptions
  }

instance HasMongoConnectionPool UserMsAppContext where
  mongoPool = _mongoPool

instance HasMongoOptions UserMsAppContext where
  mongoOptions = _mongoOptions . options

instance HasLogSettings UserMsAppContext where
  logSettings = _logSettings

instance HasRequestContext UserMsAppContext where
  requestContext = reqContext

instance HasServerOptions UserMsAppContext where
  serverOptions = _serverOptions . options

-- | Field name for record field validation errors.
newtype FieldName
  = FieldName Text
  deriving (Show)

-- | Value validator errors.
data UserValidationError
  = InvalidEmail !FieldName
  | BelowMinAge !FieldName
  deriving (Show)

data Gender = Male | Female deriving (Eq, FromJSON, Generic, Show, ToJSON)

newtype Email
  = Email Text
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

emailRegex ∷ Text
emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"

validateEmail ∷
  FieldName →
  Email →
  Validation (NonEmpty UserValidationError) Email
validateEmail fieldName em@(Email email) =
  em <$ failureUnless (email =~ emailRegex) (InvalidEmail fieldName)

validateMaybeEmail ∷
  FieldName →
  Maybe Email →
  Validation (NonEmpty UserValidationError) (Maybe Email)
validateMaybeEmail fieldName email =
  email <$ failureUnless (check email) (InvalidEmail fieldName)
  where
    check Nothing          = True
    check (Just (Email s)) = s =~ emailRegex

-- | User data type.
data User = User
  { name   :: !Text
  , age    :: !Int
  , email  :: !Email
  , gender :: !Gender
  , id     :: !(Maybe Text)
  }
  deriving (Eq, Generic, Show, ToJSON)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> do
    user <- User <$>  v .: "name"
                 <*>  v .: "age"
                 <*>  v .: "email"
                 <*>  v .: "gender"
                 <*>  v .:? "id"
    case validate user of
      Failure e -> fail $ show $ toList e
      Success u -> pure u

validateAge ∷ FieldName → Int → Validation (NonEmpty UserValidationError) Int
validateAge fieldName age = age <$ failureIf (age < 100) (BelowMinAge fieldName)

instance Validator User where
  type ValidationError User = UserValidationError
  validate User{name, age, email, gender, id} = User
    <$> Success name
    <*> validateAge (FieldName "age") age
    <*> validateEmail (FieldName "email") email
    <*> Success gender
    <*> Success id

-- | Request to update a user.
data UpdateUser = UpdateUser
  { id    :: !ObjectId
  , name  :: !(Maybe Text)
  , email :: !(Maybe Email)
  }
  deriving (Eq, Generic, Show)

instance FromJSON UpdateUser where
  parseJSON = withObject "UpdateUser" $ \v -> do
    key <- v .: "id"
    name <- v .:? "name"
    email <- v .:? "email"

    updateUser <- case readMaybe key of
      Just id -> pure $ UpdateUser {name, email, id}
      Nothing -> fail $ "invalid id " <> key

    case validate updateUser of
      Failure e -> fail $ show $ toList e
      Success u -> pure u

instance Validator UpdateUser where
  type ValidationError UpdateUser = UserValidationError
  validate UpdateUser{id, name, email} = UpdateUser
    <$> Success id
    <*> Success name
    <*> validateMaybeEmail (FieldName "email") email

instance ToJSON UpdateUser where
  toJSON UpdateUser{id, name, email} =
    object ["id" .= show id, "name" .= name, "email" .= email]

-- | Request to search for users.
data UserSearch = UserSearch
  { email  :: !(Maybe Email)
  , gender :: !(Maybe Gender)
  , name   :: !(Maybe Text)
  }
  deriving (Eq, Generic, Show, ToJSON)

instance FromJSON UserSearch where
  parseJSON = withObject "UserSearch" $ \v -> do
    userSearch <- UserSearch <$> v .:? "email"
                             <*> v .:? "gender"
                             <*> v .:? "name"
    case validate userSearch of
      Failure e -> fail $ show $ toList e
      Success u -> pure u

instance Validator UserSearch where
  type ValidationError UserSearch = UserValidationError
  validate UserSearch{email, gender, name} = UserSearch
    <$> validateMaybeEmail (FieldName "email") email
    <*> Success gender
    <*> Success name

-- BSON conversions for MongoDB.
instance Val Gender where
  val g = case g of
    Male   -> String "Male"
    Female -> String "Female"
  cast' g = case g of
    String "Male"   -> Just Male
    String "Female" -> Just Female
    _               -> Nothing

instance Val Email where
  val (Email email) = String email
  cast' email = case email of
    String s -> Just $ Email s
    _        -> Nothing

class ToBson a where
  toBson :: a -> Document

class FromBson a where
  fromBson :: MonadFail m => Document -> m a

instance ToBson User where
  toBson User{name, age, email = Email email', gender} =
    [ "name" =: name
    , "age" =: age
    , "email" =: email'
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

instance Sanitize Email where
  sanitize (Email email) = Email $ maskShowFirstLast email

instance Sanitize User where
  sanitize u@User{name, email} = u
    { name = maskShowFirstLast name
    , email = sanitize email
    }

instance Sanitize UpdateUser where
  sanitize u@UpdateUser{name, email} = u
    { name = fmap maskShowFirstLast name
    , email = fmap sanitize email
    }

instance Sanitize UserSearch where
  sanitize u@UserSearch{name, email} = u
    { name = fmap maskShowFirstLast name
    , email = fmap sanitize email
    }

data Role = Admin | NormalUser deriving (Eq, FromJSON, Generic, Show, ToJSON)

data UserClaims (r :: Role) = UserClaims
  { sub  :: !Text
  , role :: !Role
  }
  deriving (Generic, Show, ToJSON, ToJWT)

instance FromJSON (UserClaims 'Admin) where
  parseJSON = withObject "Claims" $ \v -> do
    role <- v .: "role"
    guard (role == Admin)
    sub <- v .: "sub"
    pure $ UserClaims {sub, role}

instance FromJWT (UserClaims 'Admin)

instance FromJSON (UserClaims 'NormalUser) where
  parseJSON = withObject "Claims" $ \v -> do
    role <- v .: "role"
    guard (role == NormalUser)
    sub <-v .: "sub"
    pure $ UserClaims {sub, role}

instance FromJWT (UserClaims 'NormalUser)