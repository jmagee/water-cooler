-- WaterCooler test specification.
{-# LANGUAGE TemplateHaskell #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.Internal

import           Control.Monad             (liftM2)
import           Data.Bool                 (bool)
import           Data.Time                 (NominalDiffTime, addUTCTime,
                                            diffUTCTime)
import           Path                      (Abs, Dir, File, Path, mkAbsFile,
                                            parseAbsDir, parseRelFile,
                                            toFilePath, (</>))
import           System.Directory          (doesFileExist, getCurrentDirectory,
                                            removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Property, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import           Test.QuickCheck.Monadic   (assert, monadicIO, run)

instance Arbitrary DrinkSize where
  arbitrary = oneof [pure Gulp, pure Swallow, pure Sip]
  shrink Gulp = [Swallow, Sip]
  shrink Swallow = [Sip]
  shrink Sip = []

getCWD :: IO (Path Abs Dir)
getCWD = getCurrentDirectory >>= parseAbsDir

getFileName :: String -> IO FilePath
getFileName s = do
  f <- getFileName' s
  e <- doesFileExist $ toFilePath f
  if e
    then error $ "Test file exists, please manually remove: " ++ toFilePath f
    else pure $ toFilePath f

 {-getFileName' s >>= \f -> doesFileExist f >>= checkExists-}
  {---bool (pure (toFilePath f)) (error "blah")-}
  where
    getFileName' s = liftM2 (</>) getCWD $ parseRelFile s
  {-  checkExists fp | False = pure f-}
                   {-| True  = error "blah"-}

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
      timeTilNext <- timeTilNextDrink env
      timeTilNext `shouldSatisfy` \x -> x >= -1 && x <= 1
      shouldDrink <- checkDrink env
      shouldDrink `shouldBe` True

      -- Drank, but request the next drink immediately.
      drinkWater env Sip $ Specific 0
      shouldDrink <- checkDrink env
      shouldDrink `shouldBe` True
      timeTilNext <- timeTilNextDrink env
      timeTilNext `shouldSatisfy` \x -> x >= -1 && x <= 1

      -- Drank, using the default next drink timing.
      drinkWater env Gulp Default
      shouldDrink <- checkDrink env
      shouldDrink `shouldBe` False
      timeTilNext <- timeTilNextDrink env
      timeTilNext `shouldSatisfy` \x -> x >= 1190 && x <= 1210

      -- FIXME: Do more testing here
      removeFile cooler
      removeFile history
