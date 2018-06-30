-- | The water cooler.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler
( drinkWater

  -- Re-exports.
, DrinkSize (..)
, Drink
, WaterCooler
, Optional (..) -- From Data.Optional
) where

import           WaterCooler.Env
import           WaterCooler.Internal

import           Data.Optional        (Optional (..), defaultTo)
import           Path                 (Abs, File, Path, parseAbsFile)

-- | Drink water.
drinkWater :: Env -> DrinkSize -> Optional Int -> IO ()
drinkWater (Env cooler history) size next = drink size >>= \beverage ->
  writeWaterCooler cooler $ WaterCooler beverage $ defaultTo 1200 next
{-drinkWater size next = do-}
  {-json     <- parseAbsFile "/home/jmagee/.water-cooler.json"-}
  {-beverage <- drink size-}
  {-writeWaterCooler json $ WaterCooler beverage $ defaultTo 1200 next-}
