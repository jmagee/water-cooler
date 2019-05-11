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

import           Data.Time              (LocalTime (..), UTCTime (..),
                                         getCurrentTimeZone, toGregorian,
                                         utcToLocalTime)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Git          (approxidate)

newtype FuzzyTime = FuzzyTime UTCTime
                  deriving (Show)

instance FromString FuzzyTime where
  fromString s = FuzzyTime <$> approxidate s

-- | Convert from FuzzyTime to UTCTime
toUTC :: FuzzyTime -> UTCTime
toUTC (FuzzyTime utc) = utc

-- | A date broken down into Year, Month, and Day.
type BrokenDate = (Integer, Int, Int)

-- | Convert a UTCTime into a broken down (year, month, day) tuple.
breakOutDate :: LocalTime -> BrokenDate
breakOutDate = toGregorian . localDay 

-- | Get today's date
todayDate :: IO BrokenDate
todayDate = 
  breakOutDate <$> (utcToLocalTime <$> getCurrentTimeZone <*> now)

-- | Get the current time.
now :: IO UTCTime
now = {-utcToLocalTime <$> getCurrentTimeZone <*> -}getCurrentTime
