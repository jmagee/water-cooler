-- WaterCooler test specification.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.Internal
import           WaterCooler.Util

import           Control.Monad           (liftM2)
import           Data.Sequence           (singleton)
import           Data.Time               (addUTCTime, diffUTCTime)
import           Path                    (Abs, Dir, File, Path, absfile,
                                          parseAbsDir, parseAbsFile,
                                          parseRelFile, toFilePath, (</>))
import           System.Directory        (doesFileExist, getCurrentDirectory,
                                          removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

path1 :: Path Abs File
path2 :: Path Abs File
path3 :: Path Abs File
path4 :: Path Abs File
#ifdef ming32_HOST_OS
path1 = [absfile|C:\abs\path|]
path2 = [absfile|C:\also\abs\path|]
path3 = [absfile|C:\abs\override|]
path4 = [absfile|C:\abs\override2|]
#else
path1 = [absfile|/abs/path|]
path2 = [absfile|/also/abs/path|]
path3 = [absfile|/abs/override|]
path4 = [absfile|/abs/override2|]
#endif


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
      mkEnv "/abs/path" "/also/abs/path" (singleton "nice")  "%T" "oh my" >>=
        (`shouldBe`
          Env path1
              path2
              (singleton "nice")
              "%T"
              "oh my")

  describe "mkEnv'" $
    it "create an environment without defaults " $
      mkEnv' (Specific "/abs/path")
             (Specific "/also/abs/path")
             ((Specific . singleton) "nice")
             (Specific "%F")
             (Specific "foo") >>=
        (`shouldBe`
          Env path1
              path2
              (singleton "nice")
              "%F"
              "foo")

  describe "mkEnv'" $
    it "create an environment with some defaults " $ do
      pp <- mkHomePath ".water-cooler-history" >>= parseAbsFile
      mkEnv' (Specific "/abs/path") Default Default Default Default >>=
        (`shouldBe`
          Env path1 pp drinkFlavors "%F %T" "You're thirsty")

  describe "mkEnv'" $
    it "create an environment with all defaults " $ do
      pp0 <- mkHomePath ".water-cooler" >>= parseAbsFile
      pp1 <- mkHomePath ".water-cooler-history" >>= parseAbsFile
      mkEnv' Default Default Default Default Default >>=
        (`shouldBe` Env pp0 pp1 drinkFlavors "%F %T" "You're thirsty")

  describe "Env and FakeEnv" $
    it "can round trip" $ do
      pp0 <- mkHomePath ".water-cooler"
      pp1 <- mkHomePath ".water-cooler-history"
      real <- mkEnv pp0 pp1 (singleton "foo") "%F" "foo"
      mkEnvFromFake (toFake real) >>= (`shouldBe` real)

  describe "overrideEnv" $
    it "overrides none" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo") "%F" "bar"
      overrideEnv Default Default (singleton Default) Default Default startEnv >>=
        (`shouldBe`
          Env path1
              path2
              (singleton "foo")
              "%F"
              "bar")

  describe "overrideEnv" $
    it "overrides one" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo") "%T" "bar"
      overrideEnv (Specific "/abs/override")
                  Default (singleton Default) Default Default startEnv >>=
        (`shouldBe`
          Env path3
              path2
              (singleton "foo")
              "%T"
              "bar")

  describe "overrideEnv" $
    it "overrides all" $ do
      startEnv <- mkEnv "/abs/path" "/also/abs/path" (singleton "foo") "%T" "snoo"
      overrideEnv (Specific "/abs/override")
                  (Specific "/abs/override2")
                  ((singleton . Specific) "bar")
                  (Specific "%F")
                  (Specific "boo")
                  startEnv >>=
        (`shouldBe`
          Env path3
              path4
              (singleton "bar")
              "%F"
              "boo")

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

  describe "getLastDrink" $
    it "is none" $ do
      pp0 <- Specific <$> getFileName "testFileCooler"
      pp1 <- Specific <$> getFileName "testFileHistory"
      real <- mkEnv' pp0 pp1 Default Default Default
      getLastDrink real >>= (`shouldBe` Nothing)

  describe "getLastDrink" $
    it "is a drink" $ do
      pp0 <- getFileName "testFileCooler"
      pp1 <- getFileName "testFileHistory"
      real <- mkEnv' (Specific pp0) (Specific pp1) Default Default Default
      _ <- drinkWater real (Specific Sip) (Specific 0)
      d <- drink Sip
      getLastDrink real >>= (`shouldBe` Just d)
      removeFile pp0
      removeFile pp1

  describe "readEnvRC and writeEnvRC" $
    it "work" $ do
      cooler  <- getFileName "testFileCooler"
      history <- getFileName "testFileHistory"
      rc      <- getFileName "testRC" >>= parseAbsFile
      env     <- mkEnv cooler history (singleton "flavor text") "%F" "thirsty"
      writeEnvRC rc env
      readEnvRC rc >>= (`shouldBe` env)
      removeFile (toFilePath rc)

  describe "drinkWater" $
    it "everyday" $ do
      cooler  <- getFileName "testFileCooler"
      history <- getFileName "testFileHistory"
      env     <- mkEnv cooler history drinkFlavors "%F" "thirsty"

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
