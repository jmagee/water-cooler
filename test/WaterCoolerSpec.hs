-- WaterCooler test specification.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.Internal
import           WaterCooler.Util

import           Control.Monad             (liftM2)
import           Data.Time                 (addUTCTime, diffUTCTime)
import           Path                      (Abs, Dir, Path, mkAbsFile,
                                            parseAbsDir, parseRelFile, parseAbsFile,
                                            toFilePath, (</>))
import           System.Directory          (doesFileExist, getCurrentDirectory,
                                            removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Monadic   (assert, monadicIO, run)
import Data.Sequence (singleton)

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
      mkEnv "/abs/path" "/also/abs/path" (singleton "nice") >>=
        (`shouldBe` Env $(mkAbsFile "/abs/path") $(mkAbsFile "/also/abs/path") (singleton "nice"))

  describe "mkEnv'" $
    it "create an environment without defaults " $
      mkEnv' (Specific "/abs/path") (Specific "/also/abs/path") ((Specific . singleton) "nice") >>=
        (`shouldBe` Env $(mkAbsFile "/abs/path") $(mkAbsFile "/also/abs/path") (singleton "nice"))

  describe "mkEnv'" $
    it "create an environment with some defaults " $ do
      pp <- mkHomePath ".water-cooler-history" >>= parseAbsFile
      mkEnv' (Specific "/abs/path") Default Default >>=
        (`shouldBe` Env $(mkAbsFile "/abs/path") pp drinkFlavors)

  describe "mkEnv'" $
    it "create an environment with all defaults " $ do
      pp0 <- mkHomePath ".water-cooler" >>= parseAbsFile
      pp1 <- mkHomePath ".water-cooler-history" >>= parseAbsFile
      mkEnv' Default Default Default >>= (`shouldBe` Env pp0 pp1 drinkFlavors)

  describe "Env and FakeEnv" $
    it "can round trip" $ do
      pp0 <- mkHomePath ".water-cooler"
      pp1 <- mkHomePath ".water-cooler-history"
      real <- mkEnv pp0 pp1 $ singleton "foo"
      mkEnvFromFake (toFake real) >>= (`shouldBe` real)

  describe "overrideEnv" $
    it "overrides none" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo")
      overrideEnv Default Default (singleton Default) startEnv >>=
        (`shouldBe` Env $(mkAbsFile "/abs/path") $(mkAbsFile "/also/abs/path") (singleton "foo"))

  describe "overrideEnv" $
    it "overrides one" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo")
      overrideEnv (Specific "/abs/override") Default (singleton Default) startEnv >>=
        (`shouldBe` Env $(mkAbsFile "/abs/override") $(mkAbsFile "/also/abs/path") (singleton "foo"))

  describe "overrideEnv" $
    it "overrides all" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo")
      overrideEnv (Specific "/abs/override")
                  (Specific "/abs/override2")
                  ((singleton . Specific) "bar")
                  startEnv >>=
        (`shouldBe` Env $(mkAbsFile "/abs/override") $(mkAbsFile "/abs/override2") (singleton "bar"))

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

  describe "readEnvRC and writeEnvRC" $
    it "work" $ do
      cooler  <- getFileName "testFileCooler"
      history <- getFileName "testFileHistory"
      rc      <- getFileName "testRC" >>= parseAbsFile
      env     <- mkEnv cooler history $ singleton "flavor text"
      writeEnvRC rc env
      readEnvRC rc >>= (`shouldBe` env)
      removeFile (toFilePath rc)

  describe "drinkWater" $
    it "everyday" $ do
      cooler  <- getFileName "testFileCooler"
      history <- getFileName "testFileHistory"
      env     <- mkEnv cooler history drinkFlavors

      -- FIXME: Split these out into multiple tests, otherwise it is hard
      -- to tell which one actually failed.

      -- Never drank before, expect to drink.
      timeTilNexta <- timeTilNextDrink env
      timeTilNexta `shouldSatisfy` \x -> x >= -1 && x <= 1
      shouldDrinka <- checkDrink env
      shouldDrinka `shouldBe` True

      -- Drank, but request the next drink immediately.
      flavorTextb  <- drinkWater env (Specific Sip) (Specific 0)
      flavorTextb `shouldBe` "The cool water tantalizes"
      shouldDrinkb <- checkDrink env
      shouldDrinkb `shouldBe` True
      timeTilNextb <- timeTilNextDrink env
      timeTilNextb `shouldSatisfy` \x -> x >= -1 && x <= 1

      -- Drank, using the default next drink timing.
      flavorTextc  <- drinkWater env Default Default
      flavorTextc `shouldBe` "The cool water refreshes"
      shouldDrinkc <- checkDrink env
      shouldDrinkc `shouldBe` False
      timeTilNextc <- timeTilNextDrink env
      timeTilNextc `shouldSatisfy` \x -> x >= 1190 && x <= 1210

      -- FIXME: Do more testing here
      removeFile cooler
      removeFile history
