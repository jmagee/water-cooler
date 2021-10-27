-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
, getAvgVolume
, getDaysDrinkCount
, getDaysVolume
, getWeeksDrinkCount
, getMonthDrinkCount
, getYearDrinkCount
, getHistory
, getLastDrink
, timeTilNextDrink

  -- Re-exports.
, DrinkSize (..)
, Drink
, Milliliters   -- From WaterCooler.Util
, FuzzyTime     -- From WaterCooler.FuzzyTime
, FromString    -- From WaterCooler.FromString
, fromString    -- From WaterCooler.FromString
, formatDrink   -- From WaterCooler.Internal
, Env           -- From WaterCooler.Env
, getEnvRC      -- From WaterCooler.Env
, mkEnv         -- From WaterCooler.Env
, mkEnv'        -- From WaterCooler.Env
, overrideEnv   -- From WaterCooler.Env
, toUTC         -- From WaterCooler.FuzzyTime
, putEnvRC      -- From WaterCooler.Env
, envGetCooler
, envGetHistory
, envGetDrinkText
, envGetTimeFormat
, envGetThirstyText
, todayDate     -- From WaterCooler.FuzzyTime
, WaterCooler
, Optional (..) -- From Data.Optional
, version       -- From WaterCooler.Version
) where

import           WaterCooler.Env
import           WaterCooler.FromString
import           WaterCooler.FuzzyTime
import           WaterCooler.Internal
import           WaterCooler.Util
import           WaterCooler.Version

import           Data.Optional          (Optional (..), defaultTo)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as S (Seq (Empty))
import           Data.Text              (Text)
import           Data.Time              (NominalDiffTime, diffUTCTime)
import           Data.Time.LocalTime    (getCurrentTimeZone, utcToLocalTime)

-- | Drink water.
drinkWater :: Env -> Optional DrinkSize -> Optional NominalDiffTime -> IO Text
drinkWater env size next =
  drinkWaterInternal env (defaultTo Swallow size) (defaultTo 1200 next) =<< now

-- | Check if it is time for a drink.
checkDrink :: Env -> IO Bool
checkDrink env = getCooler env >>= \case
  Just chill  -> (nextDrink chill <=) <$> now
  Nothing     -> pure True

-- | Check how long until the next drink.
timeTilNextDrink :: Env -> IO NominalDiffTime
timeTilNextDrink env = getCooler env >>= \case
  Just chill -> diffUTCTime (nextDrink chill) <$> now
  Nothing    -> let n = now in diffUTCTime <$> n <*> n

-- | Get the last drink.
getLastDrink :: Env -> IO (Maybe Drink)
getLastDrink env = lastDrink <$$> getCooler env

-- | Help to read water cooler from the environment.
getCooler :: Env -> IO (Maybe WaterCooler)
getCooler = readWaterCooler . envGetCooler

-- | Get history.
getHistory :: Env -> Optional FuzzyTime -> IO (Seq Drink)
getHistory env Default = getHistoryFiltered env $ const True
getHistory env (Specific t) = do
  local <- getCurrentTimeZone >>= \z -> pure $ utcToLocalTime z (toUTC t)
  getHistoryFiltered env (>= local)

-- | Get total water drunk on a day.
getDaysVolume :: Env -> BrokenDate -> IO Milliliters
getDaysVolume env date = sumDrinks env <$> getDaysDrinks env date

-- | Get the average amount of water drunk per day, based on full history
getAvgVolume :: Env -> IO Milliliters
getAvgVolume env = calcAvg <$> getHistory env Default
  where
    calcAvg S.Empty  = 0
    calcAvg drinks   = sumDrinks env drinks `div` length (getAllDays drinks)

-- | Get number of drinks in a day
getDaysDrinkCount :: Env -> BrokenDate -> IO Int
getDaysDrinkCount env date = length <$> getDaysDrinks env date

-- | Get number of drinks in a week
getWeeksDrinkCount :: Env -> BrokenDate -> IO Int
getWeeksDrinkCount env date = length <$> getWeeksDrinks env date

-- | Get number of drinks in the last month
getMonthDrinkCount :: Env -> BrokenDate -> IO Int
getMonthDrinkCount env date = length <$> getMonthsDrinks env date

-- | Get number of drinks in the last year
getYearDrinkCount :: Env -> BrokenDate -> IO Int
getYearDrinkCount env date = length <$> getYearsDrinks env date
