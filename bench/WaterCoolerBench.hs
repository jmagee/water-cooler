-- WaterCooler benchmarks.

module WaterCoolerBench (benchmarks, benchmarksHistory) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.Internal

import           Control.Monad             (replicateM, replicateM_)
import           Criterion.Main            (Benchmark, bench, bgroup,
                                            envWithCleanup, nf, nfIO)
import           Data.List                 (foldl')
import           Data.Sequence             (sort)
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (arbitrary)
import           Test.QuickCheck.Gen       (generate)

benchmarks :: Benchmark
benchmarks = bgroup "WaterCooler"
  [ bench "null" (nf nada 1)
  , bench "drink 1" (nfIO $ drinkALot 1)
  , bench "drink 10" (nfIO $ drinkALot 10)
  , bench "drink 100" (nfIO $ drinkALot 100)
  , bench "drink 200" (nfIO $ drinkALot 200)
  , bench "drink 300" (nfIO $ drinkALot 300)
  , bench "drink 400" (nfIO $ drinkALot 400)
  -- , bench "drink 500" (nfIO $ drinkALot 500)
  , bench "check 1" (nfIO $ checkALot 1)
  , bench "check 10" (nfIO $ checkALot 10)
  , bench "check 100" (nfIO $ checkALot 100)
  , bench "check 1000" (nfIO $ checkALot 1000)
  , bench "check 10000" (nfIO $ checkALot 10000)
  ]

benchmarksHistory :: Benchmark
benchmarksHistory = envWithCleanup createAndPopulate destroyTestEnv $ \e ->
  bgroup "WaterCoolerHistory" [ bench "history 200" (nfIO $ historyALot e 200)
                              , bench "historySort 200" (nfIO $ historySort e 200)
                              ]
  where
    createAndPopulate = do
      env <- createTestEnv "history"
      replicateM_ 1000 $ generate arbitrary >>= \drinkSize ->
        drinkWater env (Specific drinkSize) Default
      pure env

nada :: Int -> Int
nada = id

drinkALot :: Int -> IO [Text]
drinkALot n = withTestEnv "bench" $ \env ->
  replicateM n $ drinkWater env Default Default

checkALot :: Int -> IO [Bool]
checkALot n = withTestEnv "bench" $ \env ->
  drinkWater env Default Default >> replicateM n (checkDrink env)

historyALot :: Env -> Int -> IO [Drink]
historyALot env n = replicateM n $ force =<< getHistory env Default

historySort :: Env -> Int -> IO [Drink]
historySort env n = replicateM n $ force =<< (sort <$> getHistory env Default)

force :: Foldable t => t Drink -> IO Drink
force x = now >>= \n -> pure $ foldl' const (Drink Sip n) x
