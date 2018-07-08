-- | Water cooler environment.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Env
( Env (..)
, drinkFlavors
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
import           Data.Text           (Text)
import           Path                (Abs, File, Path, parseAbsFile, toFilePath)
import Data.Sequence (Seq, empty, (|>))
import qualified Data.Sequence as S (zipWith)

data Env = Env { _cooler    :: Path Abs File
               , _history   :: Path Abs File
               , _drinkText :: Seq Text
               } deriving (Eq, Show)

-- | Create an Env.
mkEnv :: MonadThrow m
      => FilePath      -- ^ Cooler file
      -> FilePath      -- ^ History file
      -> Seq Text      -- ^ Drink flavor text
      -> m Env
mkEnv a b c = do
  cooler  <- parseAbsFile a
  history <- parseAbsFile b
  pure $ Env cooler history c

-- | Create an Env with optionals.
mkEnv' :: Optional FilePath -> Optional FilePath -> Optional (Seq Text) -> IO Env
mkEnv' a b c = do
  defCooler  <- mkHomePath ".water-cooler"
  defHistory <- mkHomePath ".water-cooler-history"
  let defDrink = drinkFlavors
  mkEnv (defaultTo defCooler a)
        (defaultTo defHistory b)
        -- (mergeDrinkFlavors drinkFlavors c)
        (defaultTo defDrink c)

-- | Drink flavor texts
drinkFlavors :: Seq Text
drinkFlavors = empty
             |> "The cool water tantalizes"
             |> "The cool water refreshes"
             |> "The cool water invigorates"
             |> "Water is essential"

mergeDrinkFlavors :: Seq Text -> Seq (Optional Text) -> Seq Text
mergeDrinkFlavors a b = S.zipWith choose a b
  where
    choose x Default      = x
    choose _ (Specific y) = y

-- | Override an Env with optionals.
overrideEnv :: Optional FilePath
            -> Optional FilePath
            -> Seq (Optional Text)
            -> Env
            -> IO Env
overrideEnv a b c env =
  let cooler    = toFilePath $ _cooler env
      history   = toFilePath $ _history env
      drinkText =  _drinkText env
  in mkEnv (defaultTo cooler a)
           (defaultTo history b)
        (mergeDrinkFlavors drinkText c)
           --(defaultTo drinkText c) 

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
  FakeEnv { _fakeCooler    :: FilePath
          , _fakeHistory   :: FilePath
          , _fakeDrinkText :: Seq Text
          } deriving (Show)

instance FromJSON FakeEnv where
  parseJSON (Object v) =
    FakeEnv <$> v .: "cooler"
            <*> v .: "history"
            <*> v .: "drinkText"
  parseJSON _ = mzero

instance ToJSON FakeEnv where
  toJSON (FakeEnv cooler history drinkText) =
    object [ "cooler"    .= cooler
           , "history"   .= history
           , "drinkText" .= drinkText
           ]

-- | Create an Env from a FakeEnv
mkEnvFromFake :: MonadThrow m => FakeEnv -> m Env
mkEnvFromFake (FakeEnv a b c) = mkEnv a b c

-- | Create an Env to a FakeEnv
toFake :: Env -> FakeEnv
toFake (Env a b c) = FakeEnv (toFilePath a) (toFilePath b) c

-- | Read Env rc file
readEnvRC :: Path Abs File -> IO Env
readEnvRC file =
  join $ unlessEmpty file (mkEnv' Default Default Default) $ \contents ->
  either (jbail file) mkEnvFromFake (eitherDecode contents)

-- | Write Env rc file.
writeEnvRC:: Path Abs File -> Env -> IO ()
writeEnvRC file env = writeJSON file $ toFake env
