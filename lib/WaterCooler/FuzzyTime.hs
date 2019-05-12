-- | Fuzzy time and related time utilities.

module WaterCooler.FuzzyTime
( FuzzyTime (..)
, BrokenDate
, breakOutDate
, now
, toUTC
, todayDate
) where

import           WaterCooler.FromString

import           Data.Time                      (LocalTime (..), UTCTime (..),
                                                 getCurrentTimeZone,
                                                 toGregorian, utcToLocalTime)
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Clock                (getCurrentTime)
import           Data.Time.Git                  (approxidate)

newtype FuzzyTime = FuzzyTime UTCTime
                  deriving (Show)

instance FromString FuzzyTime where
  fromString s = FuzzyTime <$> approxidate s

-- | Convert from FuzzyTime to UTCTime
toUTC :: FuzzyTime -> UTCTime
toUTC (FuzzyTime utc) = utc

-- | A date broken down into Year, Month, and Day.
type BrokenDate = (Integer, Int, Int, Int)

-- | Convert a into a broken down (year, month, week, day) tuple.
breakOutDate :: LocalTime -> BrokenDate
breakOutDate local =
  let (y, m, d) = (toGregorian . localDay) local
      (w, _)    = (mondayStartWeek . localDay) local
  in (y, m, w, d)

-- | Get today's date
todayDate :: IO BrokenDate
todayDate = 
  breakOutDate <$> (utcToLocalTime <$> getCurrentTimeZone <*> now)

-- | Get the current time.
now :: IO UTCTime
now = {-utcToLocalTime <$> getCurrentTimeZone <*> -}getCurrentTime
