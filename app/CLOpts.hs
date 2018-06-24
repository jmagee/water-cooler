-- | Command line options.
{-# LANGUAGE OverloadedStrings #-}

module CLOpts
( parseCommandLine
, withInfo
, Options (..)
, execParser -- from Options.Applicative
) where

import           Options.Applicative

-- | Common options - these apply to multiple commands.
data Common = Common Int
            deriving (Show)

-- | Commands.
data Command = Dummy Int
             deriving (Show)

data Options = Options Common Command deriving (Show)

-- | Parse the full command line.
parseCommandLine :: Parser Options
parseCommandLine = Options <$> parseCommon <*> parseCommand

parseCommon :: Parser Common
parseCommon = pure $ Common 1

parseCommand :: Parser Command
parseCommand = pure $ Dummy 1

-- | Display help for a parser.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
