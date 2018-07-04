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
import           Options.Applicative       hiding (optional)
import           Options.Applicative.Types (readerAsk)

-- | Common options - these apply to multiple commands.
data Common = Common
                (Optional Integer) -- seconds until next drink
            deriving (Show)

-- | Commands.
data Command = DrinkWater (Optional DrinkSize)
             | Status
             | NextDrink
             | NotThirsty
             | NoWater
             deriving (Show)

data Options = Options Common Command deriving (Show)

-- | Parse the full command line.
parseCommandLine :: Parser Options
parseCommandLine = Options <$> parseCommon <*> parseCommand

parseCommon :: Parser Common
parseCommon = Common <$>
  optional (option auto $ long "wait" <> metavar "N" <> help "Set time to next drink")

parseCommand :: Parser Command
parseCommand = subparser
  $  command "drink"        (parseDrink `withInfo`  "Drink water")
  <> command "status"       (pure Status `withInfo` "Check Status")
  <> command "next"         (pure NextDrink `withInfo`   "Check next drink")
  <> command "not-thirsty"  (pure NotThirsty `withInfo`  "Not thirsty")
  <> command "no-water"     (pure NoWater `withInfo` "Out of water")

parseDrink :: Parser Command
parseDrink = DrinkWater <$> optional (argument parseDrinkSize (metavar "Drink-Size"))

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
