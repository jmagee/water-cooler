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

import           WaterCooler.Internal

import           Data.Optional            (Optional (..), defaultTo)

-- | Drink water.
drinkWater :: DrinkSize -> Optional Int -> IO ()
drinkWater size next = drink size >>= \beverage ->
  writeWaterCooler "test.json" $ WaterCooler beverage $ defaultTo 1200 next
