module Main where

import           CLOpts
import           WaterCooler
import           WaterCooler.Util

import           Data.Bool        (bool)
import           Data.Sequence    (fromList)
import qualified Data.Text.IO     as T (putStrLn)
import           Data.Time        (NominalDiffTime)

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " ++ version

run :: Options -> IO ()
run (Options (Common at cooler history sipText swallowText gulpText fakeText emptyText)
             command) = do
  let texts = fromList [sipText, swallowText, gulpText, fakeText, emptyText]
  env <- overrideEnv cooler history texts =<< getEnvRC
  case command of
    DrinkWater size ->
      drinkWater env size (fromInteger <$> at) >>= T.putStrLn

    Status          ->
      checkDrink env >>= bool (pure ()) (putStrLn "You are thirsty")

    NextDrink       ->
      timeTilNextDrink env >>= \seconds ->
        putStrLn $ "Next drink in: " ++ show seconds

    NotThirsty      ->
      drinkWater env (Specific Fake) (optionalTime 600 at) >>= T.putStrLn

    NoWater         ->
      drinkWater env (Specific Empty) (optionalTime 3600 at) >>= T.putStrLn

    Mkrc            ->
      putEnvRC env >>= \f -> putStrLn $ "Wrote " ++ f

-- | Select between a default Integer and an optional Integer and wrap the back
-- up as a Specific NominalDiffTime.
-- In other words: optionalTime 200 Default -> Specific 200
--                 optionalTime 200 (Specific 10) -> Specific 10
optionalTime :: Integer -> Optional Integer -> Optional NominalDiffTime
optionalTime a b = fromInteger <$> defaultTo' a b
