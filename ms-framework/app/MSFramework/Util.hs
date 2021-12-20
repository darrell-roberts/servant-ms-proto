module MSFramework.Util
  ( maskShowFirstLast
  , millisSinceEpoch
  , showText
  ) where

import Data.Int              (Int64)
import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Text             qualified as T
import Data.Time             (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

showText ∷ Show a ⇒ a → Text
showText = T.pack . show

millisSinceEpoch ∷ UTCTime → Int64
millisSinceEpoch =
  floor . (1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

maskShowFirstLast ∷ Text → Text
maskShowFirstLast text = fromMaybe "" $
  (\f l -> T.singleton f <> mask <> T.singleton l) <$> firstC <*> lastC
  where
    firstC = fst <$> T.uncons text
    lastC = snd <$> T.unsnoc text
    mask = T.replicate (T.length text - 2) "*"
