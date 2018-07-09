module Main where

import           Criterion.Main   (bgroup, defaultMain)
import           WaterCoolerBench

main :: IO ()
main = defaultMain [ bgroup "WaterCooler" benchmarks ]
