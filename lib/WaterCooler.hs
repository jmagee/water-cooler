-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
, timeTilNextDrink

  -- Re-exports.
, DrinkSize (..)
, Drink
, FromString    -- From WaterCooler.FromString
, fromString    -- From WaterCooler.FromString
, getEnvRC      -- From WaterCooler.Env
, mkEnv         -- From WaterCooler.Env
, mkEnv'        -- From WaterCooler.Env
, overrideEnv   -- From WaterCooler.Env
, putEnvRC      -- From WaterCooler.Env
, WaterCooler
, Optional (..) -- From Data.Optional
, version       -- From WaterCooler.Version
) where

import           WaterCooler.Env
import           WaterCooler.FromString
import           WaterCooler.Internal
import           WaterCooler.Version

import           Data.Optional          (Optional (..), defaultTo)
import           Data.Text              (Text)
import           Data.Time              (NominalDiffTime, diffUTCTime)

-- | Drink water.
drinkWater :: Env -> Optional DrinkSize -> Optional NominalDiffTime -> IO Text
drinkWater (Env coolerFile historyFile drinkText) size next = do
  let realSize = defaultTo Swallow size
  beverage <- drink realSize
  let cooler = WaterCooler beverage $ defaultTo 1200 next
  writeWaterCooler coolerFile cooler
  archiveHistory coolerFile historyFile-- fixme: archiveHistory should use cooler directly, rather than reading it
  pure $ drinkSizeToFlavor drinkText realSize

-- | Check if it is time for a drink.
checkDrink :: Env -> IO Bool
checkDrink (Env cooler _ _) = readWaterCooler cooler >>= \case
  Just chill  -> (nextDrink chill <=) <$> now
  Nothing     -> pure True

-- | Check how long until the next drink.
timeTilNextDrink :: Env -> IO NominalDiffTime
timeTilNextDrink (Env cooler _ _) = readWaterCooler cooler >>= \case
  Just chill -> diffUTCTime (nextDrink chill) <$> now
  Nothing    -> let n = now in diffUTCTime <$> n <*> n
