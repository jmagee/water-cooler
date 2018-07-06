-- | Water cooler environment.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Env
( Env (..)
, mkEnv
, mkEnv'
, mkEnvFromFake
, readEnvRC
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
import           Path                (Abs, File, Path, parseAbsFile, toFilePath)

data Env = Env { _cooler  :: Path Abs File
               , _history :: Path Abs File
               } deriving (Eq, Show)

-- | Create an Env.
mkEnv :: MonadThrow m => FilePath -> FilePath -> m Env
mkEnv a b = do
  cooler  <- parseAbsFile a
  history <- parseAbsFile b
  pure $ Env cooler history

-- | Create an Env with optionals.
mkEnv' :: Optional FilePath -> Optional FilePath -> IO Env
mkEnv' a b = do
  defCooler  <- mkHomePath ".water-cooler"
  defHistory <- mkHomePath ".water-cooler-history"
  mkEnv (defaultTo defCooler a) (defaultTo defHistory b)

-- | Fake env data used as a temporary to parse from JSON and then do
-- the File parse conversion (which involves MonadThrow.)
data FakeEnv =
  FakeEnv { _fakeCooler  :: FilePath
          , _fakeHistory :: FilePath
          } deriving (Show)

instance FromJSON (FakeEnv) where
  parseJSON (Object v) =
    FakeEnv <$> v .: "cooler"
            <*> v .: "history"
  parseJSON _ = mzero

instance ToJSON FakeEnv where
  toJSON (FakeEnv cooler history) =
    object [ "cooler"  .= cooler
           , "history" .= history
           ]

-- | Create an Env from a FakeEnv
mkEnvFromFake :: MonadThrow m => FakeEnv -> m Env
mkEnvFromFake (FakeEnv a b) = mkEnv a b

-- | Create an Env to a FakeEnv
toFake :: Env -> FakeEnv
toFake (Env a b) = FakeEnv (toFilePath a) (toFilePath b)

-- | Read Env rc file
readEnvRC :: Path Abs File -> IO Env
readEnvRC file = join $ unlessEmpty file (mkEnv' Default Default) $ \contents ->
  either (jbail file) mkEnvFromFake (eitherDecode contents)

-- | Write Env rc file.
writeEnvRC:: Path Abs File -> Env -> IO ()
writeEnvRC file env = writeJSON file $ toFake env
