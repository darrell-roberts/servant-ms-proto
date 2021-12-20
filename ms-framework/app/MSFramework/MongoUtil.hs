module MSFramework.MongoUtil
  ( asMongoKey
  , filterBsonId
  , filterBsonNull
  , runAction
  ) where

import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Text                  (Text, unpack)
import Database.MongoDB           (Action, Failure, Field (value, (:=)),
                                   ObjectId, Value (Null, ObjId), access,
                                   master)
import MSFramework.Data           (AppContext (AppContext, mongoPipe, options),
                                   ProgramOptions (collectionName))
import Text.Read                  (readMaybe)
import UnliftIO                   (MonadIO (..), liftIO, try)

-- | Convert BSON key from Text to ObjectId Type.
asMongoKey ∷ Text → Maybe ObjectId
asMongoKey = readMaybe . unpack

-- | Remove null Bson fields
filterBsonNull ∷ [Field] → [Field]
filterBsonNull = filter (\(:=){value} -> value /= Null)

-- | Remove Bson ObjectId types
filterBsonId ∷ [Field] → [Field]
filterBsonId = filter (\(:=){value} -> f value)
  where f (ObjId _) = False
        f _         = True

{-|
  Run a mongo query using settings from reader environment. `Failure`
  can be pattern matched for different error reasons:
  https://hackage.haskell.org/package/mongoDB-2.7.1.1/docs/Database-MongoDB-Query.html#t:Failure
-}
runAction ∷
  (MonadIO m, MonadReader AppContext m) ⇒
  Action IO a →
  m (Either Failure a)
runAction action = ask >>= \AppContext{mongoPipe, options} ->
    liftIO $ try $ access mongoPipe master (collectionName options) action
