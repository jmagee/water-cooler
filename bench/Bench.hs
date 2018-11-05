module Main where

import           Criterion.Main   (defaultMain)
import           WaterCoolerBench

main :: IO ()
main = defaultMain [ benchmarks
                   , benchmarksHistory
                   ]
