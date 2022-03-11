{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeFamilies #-}

module UserService.Security
  ( getJWK
  , createJwt
  , HashedUser (..)
  , Hashable (..)
  )
where

import Crypto.Hash          (Digest, SHA256)
import Crypto.Hash          qualified as Hash
import Crypto.JOSE.Error    (Error)
import Crypto.JOSE.JWK      (JWK)
import Data.Aeson           (FromJSON (..), ToJSON (..), Value (..), decode',
                             withObject, (.:))
import Data.Aeson.KeyMap    qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8)
import Data.Time.Clock      (addUTCTime, getCurrentTime,
                             secondsToNominalDiffTime)
import GHC.Generics         (Generic)
import MSFramework.Util     (showText)
import Prelude              hiding (id)
import Servant.Auth.Server  (defaultJWTSettings, makeJWT)
import UserService.Types    (Email (..), Role (..), User (..), UserAuth (..))

getJWK ∷ FilePath → IO JWK
getJWK fp =
  B.readFile fp >>=
    \case
      Just jwk -> pure jwk
      Nothing  -> fail "failed to read JWK"
      . decode'

createJwt ∷ JWK → Text → Role → IO (Either Error ByteString)
createJwt jwk subject role = do
  expires <- addUTCTime (secondsToNominalDiffTime 60 * 5) <$> getCurrentTime
  makeJWT user (defaultJWTSettings jwk) (Just expires)

  where
    user = case role of
      Admin      -> UserAuth subject role "proto-corp"
      NormalUser -> UserAuth subject role "proto-corp"

-- | A type class constraint that can be hashed.
class Hashable a where
  type Hashed a
  -- | Hash a type using the hash prefix
  hash :: Text → a → Hashed a

data HashedUser = HashedUser
  { user :: !User
  , hid  :: !Text
  }
  deriving (Generic, Show)

-- | add a "hid" hash id key value pair to a JSON object.
addHashId ∷ ToJSON a ⇒ Text → a → Value
addHashId hid v =
  Object $ KeyMap.insert "hid" (String hid) someObject
   where
    someObject = case toJSON v of
      Object o -> o
      _        -> KeyMap.empty

instance ToJSON HashedUser where
  toJSON HashedUser {hid, user} = addHashId hid user

instance FromJSON HashedUser where
  parseJSON = withObject "HashedUser" $ \v -> do
    user <- parseJSON (Object v)
    hid <- v .: "hid"
    pure HashedUser {user, hid}

sha2Hash ∷ Text → Text
sha2Hash s = let h = Hash.hash (encodeUtf8 s) :: Digest SHA256
              in showText h

instance Hashable User where
  type Hashed User = HashedUser
  hash prefix user@User {name, email = (Email email) } =
    HashedUser { user, hid = sha2Hash (prefix <> name <> email) }

instance Hashable [User] where
  type Hashed [User] = [HashedUser]
  hash prefix = fmap $ hash prefix
