module Main where

import           CLOpts
import           WaterCooler

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " -- ++ version

run :: Options -> IO ()
run = undefined
