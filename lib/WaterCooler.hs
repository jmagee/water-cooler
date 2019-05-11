-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
, getDaysDrinkCount
, getMonthDrinkCount
, getYearDrinkCount
, getHistory
, getLastDrink
, timeTilNextDrink

  -- Re-exports.
, DrinkSize (..)
, Drink
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
import           Data.Sequence          as S (filter)
import           Data.Text              (Text)
import           Data.Time              (NominalDiffTime, diffUTCTime)

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
getHistory env since = filterTime since <$> (readHistory . envGetHistory) env
 where
   filterTime (Specific t) = S.filter (compDrinkByTime $ toUTC t)
   filterTime Default      = id
   compDrinkByTime t a = _when a >= t

-- | Get number of drinks in a day
getDaysDrinkCount :: Env -> BrokenDate -> IO Int
getDaysDrinkCount env date = getSomeDrinkCount env compDrinkByDay
  where
    compDrinkByDay d = case (breakOutDate d, date) of
      ((y1, m1, d1), (y2, m2, d2)) -> (y1 == y2) && (m1 == m2) && (d1 == d2)

-- | Get number of drinks in the last month
getMonthDrinkCount :: Env -> BrokenDate -> IO Int
getMonthDrinkCount env date = getSomeDrinkCount env compDrinkByMonth
  where
    compDrinkByMonth d = case (breakOutDate d, date) of
      ((y1, m1, _), (y2, m2, _)) -> (y1 == y2) && (m1 == m2)

-- | Get number of drinks in the last year
getYearDrinkCount :: Env -> BrokenDate -> IO Int
getYearDrinkCount env date = getSomeDrinkCount env compDrinkByYear
  where
    compDrinkByYear d = case (breakOutDate d, date) of
      ((y1, _, _), (y2, _, _)) -> y1 == y2
