module Main where

import           CLOpts
import           WaterCooler

import           Data.Bool        (bool)

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " -- ++ version

run :: Options -> IO ()
run (Options common command) = do
  env <- mkEnv "/tmp/cooler.json" "/tmp/history.json"
  case command of
    DrinkWater size ->
      drinkWater env size Default >> putStrLn "The cool water refreshes"

    Status          ->
      checkDrink env >>= bool (pure ()) (putStrLn "You are thirsty")

    NextDrink       ->
      timeTilNextDrink env >>= \seconds ->
        putStrLn $ "Next drink in: " ++ show seconds

    NotThirsty      ->
      updateTimeTilNextDrink env 600 >> putStrLn "Water is essential"

    NoWater         ->
      updateTimeTilNextDrink env 3600 >> putStrLn "Fetch some more water?"
