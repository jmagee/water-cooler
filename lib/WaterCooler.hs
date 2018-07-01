-- | The water cooler.
{-# LANGUAGE LambdaCase #-}

module WaterCooler
( drinkWater
, checkDrink
, timeTilNextDrink

  -- Re-exports.
, DrinkSize (..)
, Drink
, WaterCooler
, Optional (..) -- From Data.Optional
, LocalTime     -- From Data.Time
) where

import           WaterCooler.Env
import           WaterCooler.Internal

import           Data.Optional        (Optional (..), defaultTo)
import           Data.Time            (LocalTime, NominalDiffTime, diffUTCTime,
                                       localTimeToUTC)
import           Path                 (Abs, File, Path, parseAbsFile)

-- | Drink water.
drinkWater :: Env -> DrinkSize -> Optional NominalDiffTime -> IO ()
drinkWater (Env cooler history) size next = drink size >>= \beverage -> do
  writeWaterCooler cooler $ WaterCooler beverage $ defaultTo 1200 next
  archiveHistory cooler history -- fixme: archiveHistory should use cooler directly, rather than reading it
{-drinkWater size next = do-}
  {-json     <- parseAbsFile "/home/jmagee/.water-cooler.json"-}
  {-beverage <- drink size-}
  {-writeWaterCooler json $ WaterCooler beverage $ defaultTo 1200 next-}

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
