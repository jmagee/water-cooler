module Main where

import           CLOpts
import           WaterCooler

import           Data.Bool             (bool)
import           Data.Optional         (defaultTo)
import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix (pathSeparator)

main :: IO ()
main = run =<< execParser (parseCommandLine `withInfo` infoStr)
  where
    infoStr = "The water cooler " -- ++ version

-- | Insert a system specific path separator.
slash :: FilePath -> FilePath -> FilePath
slash a b = a ++ [pathSeparator] ++ b

mkHomePath :: FilePath -> IO FilePath
mkHomePath x = (`slash` x) <$> getHomeDirectory

run :: Options -> IO ()
run (Options (Common at cooler history) command) = do
  defCooler  <- mkHomePath ".water-cooler"
  defHistory <- mkHomePath ".water-cooler-history"
  env <- mkEnv (defaultTo defCooler cooler) (defaultTo defHistory history)
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
