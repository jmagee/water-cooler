-- WaterCooler test specification.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler
import           WaterCooler.Env
import           WaterCooler.FuzzyTime
import           WaterCooler.Internal
import           WaterCooler.Util

import Data.Maybe (fromJust)
import           Control.Monad           (replicateM_)
import           Data.Sequence           (singleton)
import           Data.Sequence           as S (Seq (..))
import           Data.Time               (addUTCTime, diffUTCTime)
import           Path                    (Abs, File, Path, absfile,
                                          parseAbsFile, toFilePath)
import           System.Directory        (removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

path1 :: Path Abs File
path2 :: Path Abs File
path3 :: Path Abs File
path4 :: Path Abs File
#ifdef mingw32_HOST_OS
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

path1S :: FilePath
path1S = toFilePath path1
path2S :: FilePath
path2S = toFilePath path2
path3S :: FilePath
path3S = toFilePath path3
path4S :: FilePath
path4S = toFilePath path4

spec :: Spec
spec = do

  describe "mkEnv" $
    it "creates an environment" $
      mkEnv path1S path2S (singleton "nice")  "%T" "oh my" >>=
        (`shouldBe`
          Env path1
              path2
              (singleton "nice")
              "%T"
              "oh my")

  describe "mkEnv'" $
    it "create an environment without defaults " $
      mkEnv' (Specific path1S)
             (Specific path2S)
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
      mkEnv' (Specific path1S) Default Default Default Default >>=
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
      startEnv <- mkEnv path1S path2S (singleton "foo") "%F" "bar"
      overrideEnv Default Default (singleton Default) Default Default startEnv >>=
        (`shouldBe`
          Env path1
              path2
              (singleton "foo")
              "%F"
              "bar")

  describe "overrideEnv" $
    it "overrides one" $ do
      startEnv <- mkEnv path1S path2S (singleton "foo") "%T" "bar"
      overrideEnv (Specific path3S)
                  Default (singleton Default) Default Default startEnv >>=
        (`shouldBe`
          Env path3
              path2
              (singleton "foo")
              "%T"
              "bar")

  describe "overrideEnv" $
    it "overrides all" $ do
      startEnv <- mkEnv path1S path2S (singleton "foo") "%T" "snoo"
      overrideEnv (Specific path3S)
                  (Specific path4S)
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
      -- We don't use withTestEnv here as these files don't actually get created,
      -- and therefore cannot be removed.
      pp0 <- Specific <$> getTestFileName "testFileCooler"
      pp1 <- Specific <$> getTestFileName "testFileHistory"
      real <- mkEnv' pp0 pp1 Default Default Default
      getLastDrink real >>= (`shouldBe` Nothing)

  describe "getLastDrink" $
    it "is a drink" $ withTestEnv "test" $ \env -> do
      _ <- drinkWater env (Specific Sip) (Specific 0)
      d <- drink Sip
      getLastDrink env >>= (`shouldBe` Just d)

  describe "readEnvRC and writeEnvRC" $
    it "works" $ do
      cooler  <- getTestFileName "testFileCooler"
      history <- getTestFileName "testFileHistory"
      rc      <- getTestFileName "testRC" >>= parseAbsFile
      env     <- mkEnv cooler history (singleton "flavor text") "%F" "thirsty"
      writeEnvRC rc env
      readEnvRC rc >>= (`shouldBe` env)
      removeFile (toFilePath rc)

  describe "drinkWater" $
    it "everyday" $ withTestEnv "test" $ \env -> do
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

  describe "FuzzyTime" $ do
    it "knows January 1, 1980" $ 
      toUTC <$> fromString "January 1, 1980 00:00:00 UTC" `shouldBe`
        Just (read "1980-01-01 00:00:00")
    it "knows yesterday is before now" $ let
      yesterday =  fromJust $ fromString "yesterday"
      today = fromJust $ fromString "now"
      in toUTC yesterday `shouldSatisfy` (< toUTC today)
    it "doesn't take no garbage" $
      toUTC <$> fromString "blah bleep blo" `shouldBe` Nothing

  describe "getHistory" $ do
    it "returns empty history" $ withTestEnv "test" $ \env ->
      getHistory env Default >>= (`shouldBe` S.Empty)

    it "returns all history" $ withTestEnv "test" $ \env -> do
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      getHistory env Default >>= \l -> length l `shouldBe` 100

    it "returns all history" $ withTestEnv "test" $ \env -> do
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      getHistory env Default >>= \l -> length l `shouldBe` 100

    it "returns history since some time" $ withTestEnv "test" $ \env -> do
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      -- fromJust is evil, but ok for the test purpose here.
      lastd <- fromJust <$> getLastDrink env
      getHistory env (Specific $ (FuzzyTime . _when) lastd) >>= (`shouldBe` singleton lastd)
