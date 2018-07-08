-- | Internal buildings blocks for water cooler.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Internal
( DrinkSize (..)
, Drink (..)
, WaterCooler (..)
, archiveHistory
, drink
, drinkSizeToFlavor
, now
, magicTimeThreshold
, nextDrink
, readHistory
, readWaterCooler
, writeWaterCooler
) where

import           WaterCooler.FromString
import           WaterCooler.Util

import           Control.Monad             (mzero)
import           Data.Aeson                (FromJSON, ToJSON, Value (..),
                                            eitherDecode, object, parseJSON,
                                            toJSON, (.:), (.=))
import           Data.Char                 (toLower)
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, diffUTCTime)
import           Data.Time.Clock           (getCurrentTime)
import           Path                      (Abs, File, Path)
import           Test.QuickCheck           (oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (lookup)
import Data.Text (Text)

-- | Magic time threshold.
-- This threshold is used when comparing times.  Two times with a difference
-- within this range are considered to be effectively "the same".  This is
-- primarily used for tests.
magicTimeThreshold :: NominalDiffTime
magicTimeThreshold = 60

-- | Drink size.
-- Sip, Swallow, or Gulp- roughly small, medium, or large.
-- Fake is for a fake drink - for cases when one really doesn't want to have a
-- drink.
data DrinkSize = Fake | Sip | Swallow | Gulp
               deriving (Eq, Read, Show)

instance FromString DrinkSize where
  fromString s = case toLower <$> s of
    "sip"     -> Just Sip
    "swallow" -> Just Swallow
    "gulp"    -> Just Gulp
    "fake"    -> Just Fake
    _         -> Nothing

instance Arbitrary DrinkSize where
  arbitrary = oneof [pure Gulp, pure Swallow, pure Sip]
  shrink Gulp = [Swallow, Sip, Fake]
  shrink Swallow = [Sip, Fake]
  shrink Sip = [Fake]
  shrink Fake = []

-- | Get a flavor text string corrosponding to the drink size.
drinkSizeToFlavor :: Seq Text -> DrinkSize -> Text
drinkSizeToFlavor s Sip = maybe "Undefined" id $ S.lookup 0 s
drinkSizeToFlavor s Swallow = maybe "Undefined" id $ S.lookup 1 s
drinkSizeToFlavor s Gulp = maybe "Undefined" id $ S.lookup 2 s
drinkSizeToFlavor s Fake = maybe "Undefined" id $ S.lookup 3 s

-- | A drink - when and how much.
data Drink =
  Drink { _howMuch :: DrinkSize
        , _when    :: UTCTime
        } deriving (Show)

instance Eq Drink where
  (Drink a b) == (Drink c d) = (a == c) && (diffUTCTime b d <= magicTimeThreshold)

instance FromJSON Drink where
  -- parseJSON (Object v) = Drink <$> v .: "when" <*> (read <$> (v .: "howMuch"))
  parseJSON (Object v) = Drink <$> (read <$> (v .: "howMuch")) <*> (read <$> (v .: "when"))
  parseJSON _ = mzero

instance ToJSON Drink where
  toJSON (Drink howMuch when) =
    object [ "howMuch"    .= show howMuch
           , "when"       .= show when
           ]

-- | The keeper of fluids.
data WaterCooler =
  WaterCooler { _lastDrink      :: Drink
              , _secondsToNext  :: NominalDiffTime
              -- , _drinks         :: [Drink]
              -- Probably want to maintain drink list separately.
              } deriving (Show)

instance FromJSON WaterCooler where
  parseJSON (Object v) =
    WaterCooler <$> v .: "lastDrink"
                <*> v .: "secondsToNext"
  parseJSON _ = mzero

instance ToJSON WaterCooler where
  toJSON (WaterCooler lastDrink secondsToNext) =
    object [ "lastDrink"      .= lastDrink
           , "secondsToNext"  .= secondsToNext
           ]

-- | Drink some water now.
drink :: DrinkSize -> IO Drink
drink size = Drink size <$> now

-- | Get the current time.
now :: IO UTCTime
now = {-utcToLocalTime <$> getCurrentTimeZone <*> -}getCurrentTime

-- | Write the water cooler file.
writeWaterCooler :: Path Abs File -> WaterCooler -> IO ()
writeWaterCooler = writeJSON

-- | Read the water cooler file.
readWaterCooler :: Path Abs File -> IO (Maybe WaterCooler)
readWaterCooler file = unlessEmpty file Nothing $ \contents ->
  either (jbail file) Just (eitherDecode contents :: Either String WaterCooler)

-- | Read history.
readHistory :: Path Abs File -> IO [Drink]
readHistory file = unlessEmpty file [] $ \contents ->
  either (jbail file) id (eitherDecode contents :: Either String [Drink])

-- | Archive the water cooler history.
archiveHistory :: Path Abs File -> Path Abs File -> IO ()
archiveHistory coolFile histFile = readWaterCooler coolFile >>= \case
  Nothing                        -> pure ()
  Just (WaterCooler lastDrink _) -> do
    history <- readHistory histFile
    seq history $ writeJSON histFile $ lastDrink : history

-- | The time of the next drink
nextDrink :: WaterCooler -> UTCTime
nextDrink (WaterCooler (Drink _ lastd) next) = addUTCTime next lastd
