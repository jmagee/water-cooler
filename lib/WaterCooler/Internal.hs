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
, drinkSizeToMl
, drinkWaterInternal
, formatDrink
, getAllDays
, getDaysDrinks
, getHistoryFiltered
, getMonthsDrinks
, getWeeksDrinks
, getYearsDrinks
, lastDrink
, magicTimeThreshold
, nextDrink
, readHistory
, readWaterCooler
, sumDrinks
, writeWaterCooler
) where

import           WaterCooler.Display
import           WaterCooler.Env
import           WaterCooler.FromString
import           WaterCooler.FuzzyTime
import           WaterCooler.Util

import           Control.DeepSeq           (NFData, rnf)
import           Control.Monad             (filterM, mzero)
import           Data.Aeson                (FromJSON, ToJSON, Value (..),
                                            eitherDecode', object, parseJSON,
                                            toJSON, (.:), (.=))
import           Data.Char                 (toLower)
import           Data.Foldable             (toList)
import           Data.Maybe                (fromMaybe)
import           Data.Sequence             (Seq, fromList, (<|))
import qualified Data.Sequence             as S (Seq (..), lookup)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text, append)
import           Data.Time                 (NominalDiffTime, UTCTime (..),
                                            addUTCTime, diffUTCTime)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime       (LocalTime, getCurrentTimeZone,
                                            utcToLocalTime)
import           Path                      (Abs, File, Path)
import           Test.QuickCheck           (oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

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
-- Empty is a drink when you are out of water!
data DrinkSize = Empty | Fake | Sip | Swallow | Gulp
               deriving (Eq, Read, Ord, Show)

instance FromString DrinkSize where
  fromString s = case toLower <$> s of
    "sip"     -> Just Sip
    "swallow" -> Just Swallow
    "gulp"    -> Just Gulp
    "fake"    -> Just Fake
    "empty"   -> Just Empty
    _         -> Nothing

instance Arbitrary DrinkSize where
  arbitrary = oneof [pure Gulp, pure Swallow, pure Sip, pure Fake, pure Empty]
  shrink Gulp = [Swallow, Sip, Fake, Empty]
  shrink Swallow = [Sip, Fake, Empty]
  shrink Sip = [Fake]
  shrink Fake = [Empty]
  shrink Empty = []

instance Display DrinkSize

-- | Get a flavor text string corrosponding to the drink size.
drinkSizeToFlavor :: Seq Text -> DrinkSize -> Text
drinkSizeToFlavor s = drinkSizeTo s "Undefined"

-- | Get the amount of milliliters for a drink size.
drinkSizeToMl :: Env -> DrinkSize -> Milliliters
drinkSizeToMl env = drinkSizeToMl' (envGetDrinkVolumes env)

-- | Helper, to get the amount of milliliters for a drink size.
drinkSizeToMl' :: Seq Milliliters -> DrinkSize -> Milliliters
drinkSizeToMl' s = drinkSizeTo s 0

-- | Convert a drink size to something else, using an indexed sequence.
drinkSizeTo :: Seq a -> a -> DrinkSize -> a
drinkSizeTo s fallback Sip     = fromMaybe fallback $ S.lookup 0 s
drinkSizeTo s fallback Swallow = fromMaybe fallback $ S.lookup 1 s
drinkSizeTo s fallback Gulp    = fromMaybe fallback $ S.lookup 2 s
drinkSizeTo s fallback Fake    = fromMaybe fallback $ S.lookup 3 s
drinkSizeTo s fallback Empty   = fromMaybe fallback $ S.lookup 4 s

-- | A drink - when and how much.
data Drink =
  Drink { _howMuch :: DrinkSize
        , _when    :: UTCTime
        } deriving (Ord, Show)

instance NFData Drink where
  rnf (Drink size when) = size `seq` when `seq` ()

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
  toJSON (WaterCooler lastDrinky secondsToNext) =
    object [ "lastDrink"      .= lastDrinky
           , "secondsToNext"  .= secondsToNext
           ]

-- | Drink some water now.
drink :: DrinkSize -> IO Drink
drink size = Drink size <$> now

-- | Internal helper for drink water.
drinkWaterInternal :: Env -> DrinkSize -> NominalDiffTime -> UTCTime -> IO Text
drinkWaterInternal (Env coolerFile historyFile drinkText _ _ _) size next time = do
  let beverage = Drink size time
  let cooler = WaterCooler beverage next
  writeWaterCooler coolerFile cooler
  archiveHistory cooler historyFile
  pure $ drinkSizeToFlavor drinkText size

-- | Write the water cooler file.
writeWaterCooler :: Path Abs File -> WaterCooler -> IO ()
writeWaterCooler = writeJSON

-- | Read the water cooler file.
readWaterCooler :: Path Abs File -> IO (Maybe WaterCooler)
readWaterCooler file = unlessEmpty file Nothing $ \contents ->
  either (jbail file) Just (eitherDecode' contents :: Either String WaterCooler)

-- | Read history.
readHistory :: Path Abs File -> IO (Seq Drink)
readHistory file = unlessEmpty file S.Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: Either String (Seq Drink))

-- | Archive the water cooler history.
archiveHistory :: WaterCooler -> Path Abs File -> IO ()
archiveHistory (WaterCooler lastDrinky _) histFile = do
  history <- readHistory histFile
  seq history $ writeJSON histFile $ lastDrinky <| history

-- | Get history using the provided comparison function for filtering.
getHistoryFiltered :: Env -> (LocalTime -> Bool) -> IO (Seq Drink)
getHistoryFiltered env f = do
  history <- (readHistory . envGetHistory) env
  fromList <$> filterM f'' (toList history)
    where
      f'' x = toLocal x >>= \y -> pure (f y)
      toLocal (Drink _ t) = getCurrentTimeZone >>= \z -> pure $ utcToLocalTime z t

-- | The time of the next drink.
nextDrink :: WaterCooler -> UTCTime
nextDrink (WaterCooler (Drink _ lastd) next) = addUTCTime next lastd

-- | Get the last drink from the cooler.
lastDrink :: WaterCooler -> Drink
lastDrink = _lastDrink

-- | Format drink text.
formatDrink :: Env -> Drink -> IO Text
formatDrink env (Drink size time) = do
  zone <- getCurrentTimeZone
  let local = utcToLocalTime zone time
  let f = cs $ _timeFormat env
  pure $ build size (formatTime defaultTimeLocale f local)
    where
      build :: DrinkSize -> String -> Text
      build a b = display a `append` " at " `append` cs b

-- | Get all unique days from a sequence of Drinks.
getAllDays :: Seq Drink -> Seq UTCTime
getAllDays drinks = seqNubBy sameDay $ _when <$> drinks
  where
    sameDay :: UTCTime -> UTCTime -> Bool
    sameDay (UTCTime day1 _) (UTCTime day2 _) = day1 == day2

-- | Sum the volume of all the drinks.
sumDrinks :: Env -> Seq Drink -> Milliliters
sumDrinks env drinks = sum (drinkSizeToMl env . _howMuch <$> drinks)

-- | Get drinks in a Day.
getDaysDrinks :: Env -> BrokenDate -> IO (Seq Drink)
getDaysDrinks env date = getHistoryFiltered env compDrinkByDay
  where
    compDrinkByDay d = case (breakOutDate d, date) of
      ((y1, m1, _, d1), (y2, m2, _, d2)) -> (y1 == y2) && (m1 == m2) && (d1 == d2)

-- | Get drinks in a week.
getWeeksDrinks :: Env -> BrokenDate -> IO (Seq Drink)
getWeeksDrinks env date = getHistoryFiltered env compDrinkByWeek
  where
    compDrinkByWeek d = case (breakOutDate d, date) of
      ((y1, _, w1, _), (y2, _, w2, _)) -> (y1 == y2) && (w1 == w2)

-- | Get drinks in a month.
getMonthsDrinks :: Env -> BrokenDate -> IO (Seq Drink)
getMonthsDrinks env date = getHistoryFiltered env compDrinkByMonth
  where
    compDrinkByMonth d = case (breakOutDate d, date) of
      ((y1, m1, _, _), (y2, m2, _, _)) -> (y1 == y2) && (m1 == m2)

-- | Get drinks in a year.
getYearsDrinks :: Env -> BrokenDate -> IO (Seq Drink)
getYearsDrinks env date = getHistoryFiltered env compDrinkByYear
  where
    compDrinkByYear d = case (breakOutDate d, date) of
      ((y1, _, _, _), (y2, _, _, _)) -> y1 == y2

