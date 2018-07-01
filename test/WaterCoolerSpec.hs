-- WaterCooler test specification.
{-# LANGUAGE TemplateHaskell #-}

module WaterCoolerSpec ( spec ) where

import           WaterCooler.Env
import           WaterCooler.Internal

import           Control.Monad             (liftM2)
import           Data.Time                 (NominalDiffTime, addUTCTime,
                                            diffUTCTime)
import           Path                      (mkAbsFile)
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
