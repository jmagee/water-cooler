-- WaterCooler benchmarks.

module WaterCoolerBench (benchmarks) where

import           Criterion.Main   (Benchmark, bench, nf, nfIO)
import           Path             (Abs, Dir, File, Path, absfile, parseAbsDir,
                                   parseAbsFile, parseRelFile, toFilePath,
                                   (</>))
import           System.Directory (doesFileExist, getCurrentDirectory,
                                   removeFile)
import           WaterCooler
import           Control.Monad           (liftM2, replicateM)

benchmarks :: [Benchmark]
benchmarks = [ bench "null" (nf nada 1)
             , bench "drink 1" (nfIO $ drinkALot 1)
             , bench "drink 10" (nfIO $ drinkALot 10)
             , bench "drink 100" (nfIO $ drinkALot 100)
             , bench "drink 200" (nfIO $ drinkALot 200)
             , bench "drink 300" (nfIO $ drinkALot 300)
             , bench "drink 400" (nfIO $ drinkALot 400)
  --           , bench "drink 500" (nfIO $ drinkALot 500)
             ]

nada :: Int -> Int
nada = id

drinkALot :: Int -> IO ()
drinkALot n = do
  cooler  <- getFileName "benchFileCooler"
  history <- getFileName "benchFileHistory"
  env     <- mkEnv' (Specific cooler) (Specific history) Default Default Default

  _ <- replicateM n $ drinkWater env Default Default
  removeFile cooler
  removeFile history

getCWD :: IO (Path Abs Dir)
getCWD = getCurrentDirectory >>= parseAbsDir

getFileName :: String -> IO FilePath
getFileName s = do
  f <- getFileName' s
  e <- doesFileExist $ toFilePath f
  if e
    then error $ "Test file exists, please manually remove: " ++ toFilePath f
    else pure $ toFilePath f
  where
    getFileName' = liftM2 (</>) getCWD . parseRelFile
