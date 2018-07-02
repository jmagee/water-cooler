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
import           Options.Applicative       hiding (optional)
import           Options.Applicative.Types (readerAsk)

-- | Common options - these apply to multiple commands.
data Common = Common Int
            deriving (Show)

-- | Commands.
data Command = DrinkWater (Optional DrinkSize)
             deriving (Show)

data Options = Options Common Command deriving (Show)

-- | Parse the full command line.
parseCommandLine :: Parser Options
parseCommandLine = Options <$> parseCommon <*> parseCommand

parseCommon :: Parser Common
parseCommon = pure $ Common 1

parseCommand :: Parser Command
parseCommand = subparser
  $ command "drink"        (parseDrink `withInfo` "Drink water")

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
