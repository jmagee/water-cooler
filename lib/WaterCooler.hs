-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
, timeTilNextDrink
, updateTimeTilNextDrink

  -- Re-exports.
, DrinkSize (..)
, Drink
, FromString    -- From WaterCooler.FromString
, fromString    -- From WaterCooler.FromString
, mkEnv         -- From WaterCooler.Env
, WaterCooler
, Optional (..) -- From Data.Optional
) where

import           WaterCooler.Env
import           WaterCooler.FromString
import           WaterCooler.Internal

import           Data.Optional          (Optional (..), defaultTo)
import           Data.Time              (NominalDiffTime, diffUTCTime)

-- | Drink water.
drinkWater :: Env -> Optional DrinkSize -> Optional NominalDiffTime -> IO ()
drinkWater (Env coolerFile historyFile) size next = do
  let realSize = defaultTo Swallow size
  beverage <- drink realSize
  let cooler = WaterCooler beverage $ defaultTo 1200 next
  writeWaterCooler coolerFile cooler
  archiveHistory coolerFile historyFile-- fixme: archiveHistory should use cooler directly, rather than reading it

-- | Check if it is time for a drink.
checkDrink :: Env -> IO Bool
checkDrink (Env cooler _) = readWaterCooler cooler >>= \case
  Just chill  -> (nextDrink chill <=) <$> now
  Nothing     -> pure True

-- | Check how long until the next drink.
timeTilNextDrink :: Env -> IO NominalDiffTime
timeTilNextDrink (Env cooler _) = readWaterCooler cooler >>= \case
  Just chill -> diffUTCTime (nextDrink chill) <$> now
  Nothing    -> let n = now in diffUTCTime <$> n <*> n

-- | Update the time until the next drink.
updateTimeTilNextDrink :: Env -> NominalDiffTime -> IO ()
updateTimeTilNextDrink env t = drinkWater env (Specific Fake) (Specific t)
