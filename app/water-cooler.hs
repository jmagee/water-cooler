{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           CLOpts
import           WaterCooler
import           WaterCooler.Util

import           Data.Bool     (bool)
import           Data.Sequence (Seq, fromList)
import           Data.Text     (Text)
import qualified Data.Text.IO  as T (putStrLn)
import           Data.Time     (NominalDiffTime)

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " ++ version

run :: Options -> IO ()
run (Options com command) =
  getEnvRC >>= overrideEnv (_cooler com)
                           (_history com)
                           (extractFlavors com)
                           (extractVolumes com)
                           (_timeFormat com)
                           (_thirstyText com)
           >>= \env -> dispatchCommand env command $ _wait com

dispatchCommand :: Env -> Command -> Optional Integer -> IO ()
dispatchCommand env (DrinkWater size) wait =
  drinkWater env size (fromInteger <$> wait) >>= T.putStrLn

dispatchCommand env Status _ =
  checkDrink env >>= bool (pure ()) (T.putStrLn (envGetThirstyText env))

dispatchCommand env NextDrink _ =
  timeTilNextDrink env >>= \seconds -> putStrLn $ "Next drink in: " ++
                                                  secondsToHumanString seconds

dispatchCommand env LastDrink _ =
  getLastDrink env >>= \case
    Nothing -> putStrLn "None"
    Just d  -> T.putStrLn =<< formatDrink env d

dispatchCommand env NotThirsty wait =
  T.putStrLn =<< drinkWater env (Specific Fake) (optionalTime 600 wait)

dispatchCommand env NoWater wait =
  T.putStrLn =<< drinkWater env (Specific Empty) (optionalTime 3600 wait)

dispatchCommand env Mkrc _ =
  (\f -> putStrLn $ "Wrote " ++ f) =<< putEnvRC env

dispatchCommand env (History since) _ =
  getHistory env since >>= mapM_ formatAndPrint
  where
    formatAndPrint x = formatDrink env x >>= T.putStrLn

dispatchCommand env Stats _ = do
  today <- todayDate
  (\x -> putStrLn $ "# of all time drinks: " ++ show x) =<< length <$> getHistory env Default
  (\x -> putStrLn $ "# of drinks this year: " ++ show x) =<< getYearDrinkCount env today
  (\x -> putStrLn $ "# of drinks this month: " ++ show x) =<< getMonthDrinkCount env today
  (\x -> putStrLn $ "# of drinks this week: " ++ show x) =<< getWeeksDrinkCount env today
  (\x -> putStrLn $ "# of drinks today: " ++ show x) =<< getDaysDrinkCount env today
  (\x -> putStrLn $ "\nVolume consumed today: " ++ show x ++ "ml") =<< getDaysVolume env today
  (\x -> putStrLn $ "Average daily consumption: " ++ show x ++ "ml") =<< getAvgVolume env

-- | Select between a default Integer and an optional Integer and wrap the back
-- up as a Specific NominalDiffTime.
-- In other words: optionalTime 200 Default -> Specific 200
--                 optionalTime 200 (Specific 10) -> Specific 10
optionalTime :: Integer -> Optional Integer -> Optional NominalDiffTime
optionalTime a b = fromInteger <$> defaultTo' a b

-- | Extract DrinkSize flavor texts from Common options.
extractFlavors :: Common -> Seq (Optional Text)
extractFlavors com =
  fromList (($ com) <$> [_sipText, _swallowText, _gulpText, _fakeText, _emptyText])

-- | Extract Drink size volumes from Common options.
extractVolumes :: Common -> Seq (Optional Milliliters)
extractVolumes com =
  fromList (($ com) <$> [_sipVol, _swallowVol, _gulpVol, _fakeVol, _emptyVol])
