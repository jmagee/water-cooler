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

import           Control.Monad           (replicateM_)
import           Data.Foldable           (toList)
import           Data.Functor.Identity   (runIdentity)
import           Data.List               (nub, nubBy)
import           Data.Maybe              (fromJust)
import           Data.Sequence           (fromList, singleton)
import           Data.Sequence           as S (Seq (..))
import           Data.Time               (UTCTime, addUTCTime,
                                          defaultTimeLocale, diffUTCTime,
                                          parseTimeM, LocalTime)
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

-- | Force a UTCTime from a fuzzy string.
-- For testing convince only, since it is partial.
forceUTCString :: String -> UTCTime
forceUTCString = toUTC . fromJust . fromString

-- | Force a UTCTime from a "YYYY-mm-dd" string.
forceUTCDate:: String -> UTCTime
forceUTCDate= runIdentity . parseTimeM False defaultTimeLocale "%Y-%m-%d"

-- | Force a Localtime from a "YYYY-mm-dd" string.
forceLocalDate:: String -> LocalTime
forceLocalDate= runIdentity . parseTimeM False defaultTimeLocale "%Y-%m-%d"

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
      yesterday =  forceUTCString "yesterday"
      today = forceUTCString "now"
      in yesterday `shouldSatisfy` (< today)
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

  describe "seqNubBy" $ do
    prop "is equivelant to nub" $ \x ->
      let s = fromList (x :: [Int])
      in toList (seqNubBy (==) s) `shouldBe` nub x
    prop "is equivelant to nubBy" $ \x ->
      let s = fromList (x :: [String])
      in toList (seqNubBy (/=) s) `shouldBe` nubBy (/=) x
    prop "is equivelant to nubBy with drink sizes" $ \x ->
      let s = fromList (x :: [DrinkSize])
      in toList (seqNubBy (/=) s) `shouldBe` nubBy (/=) x

  describe "maybeToOptional" $
    prop "is good" $ \x ->
      let o = maybeToOptional x
      in case (o, x) of
          (Specific a, Just b)  -> a `shouldBe` (b :: Int)
          (Default,    Nothing) -> True `shouldBe` True
          (_, _) -> expectationFailure "mismatch between Optional and Maybe"

  describe "getAllDays" $
    it "seems somewhat reasonable" $ let
      drinks = fromList [ Drink Sip (forceUTCString "yesterday")
                        , Drink Sip (forceUTCString "2 days")
                        , Drink Sip (forceUTCString "2 days")
                        , Drink Sip (forceUTCString "yesterday")
                        , Drink Sip (forceUTCString "yesterday")
                        , Drink Sip (forceUTCString "yesterday")
                        , Drink Sip (forceUTCString "2 days")
                        ]
      days = getAllDays drinks
      in length days `shouldBe` 2

  describe "breakOutDate" $ do
    it "2018-01-30" $
      breakOutDate (forceLocalDate "2018-01-30") `shouldBe` (2018, 1, 30)
    it "2020-08-11" $
      breakOutDate (forceLocalDate "2020-08-20") `shouldBe` (2020, 8, 20)
    it "1970-12-31" $
      breakOutDate (forceLocalDate "1970-12-31") `shouldBe` (1970, 12, 31)
    it "1971-01-01" $
      breakOutDate (forceLocalDate "1971-01-01") `shouldBe` (1971, 01, 01)

  describe "getDayDrinkCount" $ do
    it "doesn't count old day" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      getDaysDrinkCount env (2018, 1, 1) >>= (`shouldBe` 0)
    it "seems to add up" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      today <- todayDate
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      getDaysDrinkCount env today >>= (`shouldBe` 100)

  describe "getMonthDrinkCount" $ do
    it "doesn't count old months" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      getMonthDrinkCount env (2018, 1, 1) >>= (`shouldBe` 0)
    it "seems to add up" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      today <- todayDate
      getMonthDrinkCount env today >>= (`shouldBe` 100)

  describe "getYearDrinkCount" $ do
    it "doesn't count old years" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      getYearDrinkCount env (2018, 1, 1) >>= (`shouldBe` 0)
    it "seems to add up" $ withTestEnv "test" $ \env -> do
      _ <- drinkWaterInternal env Swallow 1200 $ forceUTCDate "2000-01-01"
      replicateM_ 100 $ drinkWater env (Specific Sip) (Specific 0)
      today <- todayDate
      getYearDrinkCount env today >>= (`shouldBe` 100)
