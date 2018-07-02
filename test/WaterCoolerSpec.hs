-- WaterCooler test specification.
{-# LANGUAGE TemplateHaskell #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.Internal

import           Control.Monad             (liftM2)
import           Data.Time                 (addUTCTime, diffUTCTime)
import           Path                      (Abs, Dir, Path, mkAbsFile,
                                            parseAbsDir, parseRelFile,
                                            toFilePath, (</>))
import           System.Directory          (doesFileExist, getCurrentDirectory,
                                            removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Monadic   (assert, monadicIO, run)

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

spec :: Spec
spec = do

  describe "mkEnv" $
    it "creates an environment" $
      mkEnv "/abs/path" "/also/abs/path" >>=
        (`shouldBe` Env $(mkAbsFile "/abs/path") $(mkAbsFile "/also/abs/path"))

  describe "now" $
    it "seems somewhat sane" $ do
      diff <- diffUTCTime <$> now <*> now
      diff `shouldSatisfy` (< magicTimeThreshold)
      -- Not deterministic, but is a pretty big grace period.

  describe "drink" $
    prop "is refreshing" $ \x -> monadicIO $ do
      d1 <- run (drink x)
      d2 <- run (Drink x <$> now)
      assert $ d1 == d2

  describe "nextDrink" $
    prop "is soon" $ \s t -> monadicIO $ do
      let t' = fromInteger t
      howSoonIs    <- run now
      wc           <- run $ flip WaterCooler t' <$> drink s
      let expected = addUTCTime t' howSoonIs
      let actual   = nextDrink wc
      assert $ diffUTCTime expected actual < magicTimeThreshold

  describe "drinkWater" $
    it "everyday" $ do
      cooler  <- getFileName "testFileCooler"
      history <- getFileName "testFileHistory"
      env     <- mkEnv cooler history

      -- Never drank before, expect to drink.
      timeTilNexta <- timeTilNextDrink env
      timeTilNexta `shouldSatisfy` \x -> x >= -1 && x <= 1
      shouldDrinka <- checkDrink env
      shouldDrinka `shouldBe` True

      -- Drank, but request the next drink immediately.
      drinkWater env Sip $ Specific 0
      shouldDrinkb <- checkDrink env
      shouldDrinkb `shouldBe` True
      timeTilNextb <- timeTilNextDrink env
      timeTilNextb `shouldSatisfy` \x -> x >= -1 && x <= 1

      -- Drank, using the default next drink timing.
      drinkWater env Gulp Default
      shouldDrinkc <- checkDrink env
      shouldDrinkc `shouldBe` False
      timeTilNextc <- timeTilNextDrink env
      timeTilNextc `shouldSatisfy` \x -> x >= 1190 && x <= 1210

      -- FIXME: Do more testing here
      removeFile cooler
      removeFile history
