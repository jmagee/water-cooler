-- | Water cooler environment.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Env
( Env (..)
, drinkFlavors
, envGetCooler
, envGetHistory
, envGetDrinkText
, envGetTimeFormat
, envGetThirstyText
, getEnvRC
, mergeDrinkFlavors
, mkEnv
, mkEnv'
, mkEnvFromFake
, readEnvRC
, overrideEnv
, putEnvRC
, toFake
, writeEnvRC
) where

import           WaterCooler.Util

import           Control.Monad       (join, mzero)
import           Control.Monad.Catch (MonadThrow)
import           Data.Aeson          (FromJSON, ToJSON, Value (..),
                                      eitherDecode, object, parseJSON, toJSON,
                                      (.:), (.=))
import           Data.Optional       (Optional (..), defaultTo)
import           Data.Sequence       (Seq, empty, (|>))
import qualified Data.Sequence       as S (zipWith)
import           Data.Text           (Text)
import           Path                (Abs, File, Path, parseAbsFile, toFilePath)

data Env = Env { _cooler      :: Path Abs File
               , _history     :: Path Abs File
               , _drinkText   :: Seq Text
               , _timeFormat  :: Text
               , _thirstyText :: Text
               } deriving (Eq, Show)

-- | Create an Env.
mkEnv :: MonadThrow m
      => FilePath      -- ^ Cooler file
      -> FilePath      -- ^ History file
      -> Seq Text      -- ^ Drink flavor text
      -> Text          -- ^ Time format string
      -> Text          -- ^ Thirsty flavor text
      -> m Env
mkEnv a b c d e = do
  cooler  <- parseAbsFile a
  history <- parseAbsFile b
  pure $ Env cooler history c d e

-- | Create an Env with optionals.
mkEnv' :: Optional FilePath
       -> Optional FilePath
       -> Optional (Seq Text)
       -> Optional Text
       -> Optional Text
       -> IO Env
mkEnv' a b c d e = do
  defCooler  <- mkHomePath ".water-cooler"
  defHistory <- mkHomePath ".water-cooler-history"
  let defDrink = drinkFlavors
  let defTime = "%F %T"
  let defThirsty = "You're thirsty"
  mkEnv (defaultTo defCooler a)
        (defaultTo defHistory b)
        (defaultTo defDrink c)
        (defaultTo defTime d)
        (defaultTo defThirsty e)

-- FIXME: Should this be moved closer to drinkSize?
-- | Drink flavor texts
drinkFlavors :: Seq Text
drinkFlavors = empty
             |> "The cool water tantalizes"
             |> "The cool water refreshes"
             |> "The cool water invigorates"
             |> "Water is essential"
             |> "Fetch some more water?"

mergeDrinkFlavors :: Seq Text -> Seq (Optional Text) -> Seq Text
mergeDrinkFlavors  = S.zipWith choose
  where
    choose x Default      = x
    choose _ (Specific y) = y

-- | Override an Env with optionals.
overrideEnv :: Optional FilePath
            -> Optional FilePath
            -> Seq (Optional Text)
            -> Optional Text
            -> Optional Text
            -> Env
            -> IO Env
overrideEnv a b c d e env =
  let cooler    = toFilePath $ _cooler env
      history   = toFilePath $ _history env
      drinkText =  _drinkText env
      dt        = _timeFormat env
      thirstText= _thirstyText env
  in mkEnv (defaultTo cooler a)
           (defaultTo history b)
           (mergeDrinkFlavors drinkText c)
           (defaultTo dt d)
           (defaultTo thirstText e)

-- | Get Env from RC file
getEnvRC :: IO Env
getEnvRC = mkHomePath ".water-cooler.rc" >>= parseAbsFile >>= readEnvRC

-- | Put Env to a RC file
putEnvRC :: Env -> IO FilePath
putEnvRC env = do
  rc   <- mkHomePath ".water-cooler.rc"
  file <- parseAbsFile rc
  writeEnvRC file env
  pure rc

-- | Fake env data used as a temporary to parse from JSON and then do
-- the File parse conversion (which involves MonadThrow.)
data FakeEnv =
  FakeEnv { _fakeCooler      :: FilePath
          , _fakeHistory     :: FilePath
          , _fakeDrinkText   :: Seq Text
          , _fakeTimeFormat  :: Text
          , _fakethirstyText :: Text
          } deriving (Show)

instance FromJSON FakeEnv where
  parseJSON (Object v) =
    FakeEnv <$> v .: "cooler"
            <*> v .: "history"
            <*> v .: "drinkText"
            <*> v .: "timeFormat"
            <*> v .: "thirstyText"
  parseJSON _ = mzero

instance ToJSON FakeEnv where
  toJSON (FakeEnv cooler history drinkText timeFormat thirstyText) =
    object [ "cooler"      .= cooler
           , "history"     .= history
           , "drinkText"   .= drinkText
           , "timeFormat"  .= timeFormat
           , "thirstyText" .= thirstyText
           ]

-- | Create an Env from a FakeEnv
mkEnvFromFake :: MonadThrow m => FakeEnv -> m Env
mkEnvFromFake (FakeEnv a b c d e) = mkEnv a b c d e

-- | Create an Env to a FakeEnv
toFake :: Env -> FakeEnv
toFake (Env a b c d e) = FakeEnv (toFilePath a) (toFilePath b) c d e

-- | Read Env rc file
readEnvRC :: Path Abs File -> IO Env
readEnvRC file = join $
  unlessEmpty file (mkEnv' Default Default Default Default Default) $ \contents ->
  either (jbail file) mkEnvFromFake (eitherDecode contents)

-- | Write Env rc file.
writeEnvRC:: Path Abs File -> Env -> IO ()
writeEnvRC file env = writeJSON file $ toFake env


-- Public Env getters.
-- | Get the cooler from the environment.
envGetCooler :: Env -> Path Abs File
envGetCooler = _cooler

-- | Get the history from the environment.
envGetHistory :: Env -> Path Abs File
envGetHistory = _history

-- | Get the drink text from the environment.
envGetDrinkText:: Env -> Seq Text
envGetDrinkText = _drinkText

-- | Get the time format from the environment.
envGetTimeFormat :: Env -> Text
envGetTimeFormat = _timeFormat

-- | Get the thirsty text from the environment.
envGetThirstyText :: Env -> Text
envGetThirstyText = _thirstyText
