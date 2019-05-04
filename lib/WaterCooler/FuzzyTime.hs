-- | Fuzzy time

module WaterCooler.FuzzyTime
( FuzzyTime (..)
, toUTC
) where

import           WaterCooler.FromString

import           Data.Time              (UTCTime)
import           Data.Time.Git          (approxidate)

newtype FuzzyTime = FuzzyTime UTCTime
                  deriving (Show)

instance FromString FuzzyTime where
  fromString s = FuzzyTime <$> approxidate s

-- | Convert from FuzzyTime to UTCTime
toUTC :: FuzzyTime -> UTCTime
toUTC (FuzzyTime utc) = utc
