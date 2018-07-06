module Main where

import           CLOpts
import           WaterCooler

import           Data.Bool             (bool)
import           Data.Optional         (defaultTo)

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " ++ version

run :: Options -> IO ()
run (Options (Common at cooler history) command) = do
  env <- overrideEnv cooler history =<< getEnvRC
  case command of
    DrinkWater size ->
      drinkWater env size (fromInteger <$> at) >> putStrLn "The cool water refreshes"

    Status          ->
      checkDrink env >>= bool (pure ()) (putStrLn "You are thirsty")

    NextDrink       ->
      timeTilNextDrink env >>= \seconds ->
        putStrLn $ "Next drink in: " ++ show seconds

    NotThirsty      ->
      updateTimeTilNextDrink env (fromInteger (defaultTo 600 at)) >>
        putStrLn "Water is essential"

    NoWater         ->
      updateTimeTilNextDrink env (fromInteger (defaultTo 3600 at)) >>
        putStrLn "Fetch some more water?"

    Mkrc            ->
      putEnvRC env >>= \f -> putStrLn $ "Wrote " ++ f
