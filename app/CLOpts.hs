-- | Command line options.
{-# LANGUAGE OverloadedStrings #-}

module CLOpts
( parseCommandLine
, withInfo
, Options (..)
, Command (..)
, Common (..)
, execParser -- from Options.Applicative
) where

import           WaterCooler

import           Data.Optional             (Optional (..))
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import           Options.Applicative       hiding (optional)
import           Options.Applicative.Types (readerAsk)

-- | Common options - these apply to multiple commands.
data Common = Common
            { _wait    :: Optional Integer  -- seconds until next drink
            , _cooler  :: Optional FilePath -- Env cooler location
            , _history :: Optional FilePath -- Env history location
            , _sipText :: Optional Text     -- Env drink sip text
            , _swallowText :: Optional Text     -- Env drink swallow text
            , _gulpText    :: Optional Text     -- Env drink gulp text
            , _fakeText    :: Optional Text     -- Env drink fake text
            , _emptyText   :: Optional Text     -- Env drink empty text
            , _thirstyText :: Optional Text     -- Env thirsty text
            , _timeFormat  :: Optional Text     -- Env date/time format text
            } deriving (Show)

-- | Commands.
data Command = DrinkWater (Optional DrinkSize)
             | Status
             | NextDrink
             | LastDrink
             | NotThirsty
             | NoWater
             | Mkrc
             | History
             deriving (Show)

data Options = Options Common Command deriving (Show)

-- | Parse the full command line.
parseCommandLine :: Parser Options
parseCommandLine = vers *> (Options <$> parseCommon <*> parseCommand)
  where
    vers = infoOption version (  long "version"
                              <> short 'v'
                              <> help "Display version"
                              <> hidden)

parseCommon :: Parser Common
parseCommon = Common
  <$> optional (option auto $ long "wait"
                            <> metavar "N"
                            <> help "Set time to next drink")
  <*> optional (strOption $ long "env-cooler"
                          <> metavar "path"
                          <> help "Absolute path of cooler file"
                          <> hidden)
  <*> optional (strOption $ long "env-history"
                          <> metavar "path"
                          <> help "Absolute path of history file"
                          <> hidden)
  <*> optional (strOption $ long "env-sip-text"
                          <> metavar "flavor text"
                          <> help "Message to display after sipping"
                          <> hidden)
  <*> optional (strOption $ long "env-swallow-text"
                          <> metavar "flavor text"
                          <> help "Message to display after swallowing"
                          <> hidden)
  <*> optional (strOption $ long "env-gulp-text"
                          <> metavar "flavor text"
                          <> help "Message to display after gulping"
                          <> hidden)
  <*> optional (strOption $ long "env-fake-text"
                          <> metavar "flavor text"
                          <> help "Message to display after a fake drink"
                          <> hidden)
  <*> optional (strOption $ long "env-empty-text"
                          <> metavar "flavor text"
                          <> help "Message to display when out of water"
                          <> hidden)
  <*> optional (strOption $ long "env-thirsty-text"
                          <> metavar "flavor text"
                          <> help "Message to display when thirsty"
                          <> hidden)
  <*> optional (strOption $ long "env-timeformat"
                          <> metavar "time format"
                          <> help "Unix-style date/time format string"
                          <> hidden)

parseCommand :: Parser Command
parseCommand = subparser
  $  command "drink"        (parseDrink `withInfo`  "Drink water")
  <> command "status"       (pure Status `withInfo` "Check Status")
  <> command "next"         (pure NextDrink `withInfo`   "Check next drink")
  <> command "last"         (pure LastDrink `withInfo`   "Check last drink")
  <> command "not-thirsty"  (pure NotThirsty `withInfo`  "Not thirsty")
  <> command "no-water"     (pure NoWater `withInfo` "Out of water")
  <> command "mkrc"         (pure Mkrc `withInfo` "Create RC file")
  <> command "history"         (pure History `withInfo` "Recall drink history")

parseDrink :: Parser Command
parseDrink = DrinkWater <$> optional (argument parseDrinkSize $
  metavar "Drink-Size" <> completeWith ["sip", "swallow", "gulp"])

parseDrinkSize :: ReadM DrinkSize
parseDrinkSize = parseCustom "Drink-Size must be 'sip', 'swallow', or 'gulp'."

-- | Helper to parse custom types.
parseCustom :: FromString s => String -> ReadM s
parseCustom err = readerAsk >>= \x -> case fromString x of
  Just r  -> pure r
  Nothing -> readerAbort $ ErrorMsg err

-- | Equivalent to optional from Control.Applicative, but return Data.Optional
-- instead of Maybe.
optional :: Alternative f => f a -> f (Optional a)
optional v = Specific <$> v <|> pure Default

-- | Display help for a parser.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
