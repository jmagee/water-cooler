-- WaterCooler benchmarks.

module WaterCoolerBench (benchmarks) where

import           WaterCooler
import           WaterCooler.Env

import           Control.Monad    (replicateM, void)
import           Criterion.Main   (Benchmark, bench, nf, nfIO)

benchmarks :: [Benchmark]
benchmarks = [ bench "null" (nf nada 1)
             , bench "drink 1" (nfIO $ drinkALot 1)
             , bench "drink 10" (nfIO $ drinkALot 10)
             , bench "drink 100" (nfIO $ drinkALot 100)
             , bench "drink 200" (nfIO $ drinkALot 200)
             , bench "drink 300" (nfIO $ drinkALot 300)
             , bench "drink 400" (nfIO $ drinkALot 400)
  --           , bench "drink 500" (nfIO $ drinkALot 500)
             , bench "check 1" (nfIO $ checkALot 1)
             , bench "check 10" (nfIO $ checkALot 10)
             , bench "check 100" (nfIO $ checkALot 100)
             , bench "check 1000" (nfIO $ checkALot 1000)
             , bench "check 10000" (nfIO $ checkALot 10000)
             ]

nada :: Int -> Int
nada = id

drinkALot :: Int -> IO ()
drinkALot n = withTestEnv "bench" $ \env ->
  void $ replicateM n $ drinkWater env Default Default

checkALot :: Int -> IO ()
checkALot n = withTestEnv "bench" $ \env ->
  drinkWater env Default Default >> void (replicateM n $ checkDrink env)


