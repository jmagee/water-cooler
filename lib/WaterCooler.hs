-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
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
drinkWater (Env coolerFile historyFile drinkText _ _) size next = do
  let realSize = defaultTo Swallow size
  beverage <- drink realSize
  let cooler = WaterCooler beverage $ defaultTo 1200 next
  writeWaterCooler coolerFile cooler
  archiveHistory cooler historyFile
  pure $ drinkSizeToFlavor drinkText realSize

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
