-- | Water cooler environment.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Env
( Env (..)
, drinkFlavors
, drinkVolumes
, envGetCooler
, envGetHistory
, envGetDrinkText
, envGetDrinkVolumes
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

-- Only for testing.
, createTestEnv
, destroyTestEnv
, withTestEnv
) where

import           WaterCooler.Util

import           Control.DeepSeq     (NFData, rnf)
import           Control.Monad       (join, mzero, when)
import           Control.Monad.Catch (MonadThrow)
import           Data.Aeson          (FromJSON, ToJSON, Value (..),
                                      eitherDecode, object, parseJSON, toJSON,
                                      (.:), (.=))
import           Data.Optional       (Optional (..), defaultTo)
import           Data.Sequence       (Seq, empty, (|>))
import qualified Data.Sequence       as S (zipWith)
import           Data.Text           (Text)
import           Path                (Abs, File, Path, parseAbsFile, toFilePath)
import           System.Directory    (doesFileExist, removeFile)

data Env = Env { _cooler      :: Path Abs File
               , _history     :: Path Abs File
               , _drinkText   :: Seq Text
               , _drinkVolume :: Seq Milliliters
               , _timeFormat  :: Text
               , _thirstyText :: Text
               } deriving (Eq, Show)

instance NFData Env where
  rnf (Env a b c d e f) = a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()

-- | Create an Env.
mkEnv :: MonadThrow m
      => FilePath        -- ^ Cooler file
      -> FilePath        -- ^ History file
      -> Seq Text        -- ^ Drink flavor text
      -> Seq Milliliters -- ^ Drink volumes 
      -> Text            -- ^ Time format string
      -> Text            -- ^ Thirsty flavor text
      -> m Env
mkEnv a b c d e f = do
  cooler  <- parseAbsFile a
  history <- parseAbsFile b
  pure $ Env cooler history c d e f

-- | Create an Env with optionals.
mkEnv' :: Optional FilePath
       -> Optional FilePath
       -> Optional (Seq Text)
       -> Optional (Seq Milliliters)
       -> Optional Text
       -> Optional Text
       -> IO Env
mkEnv' a b c d e f = do
  defCooler  <- mkHomePath ".water-cooler"
  defHistory <- mkHomePath ".water-cooler-history"
  let defDrink = drinkFlavors
  let defVolume = drinkVolumes
  let defTime = "%F %T"
  let defThirsty = "You're thirsty"
  mkEnv (defaultTo defCooler a)
        (defaultTo defHistory b)
        (defaultTo defDrink c)
        (defaultTo defVolume d)
        (defaultTo defTime e)
        (defaultTo defThirsty f)

-- FIXME: Should this be moved closer to drinkSize?
-- | Drink flavor texts
drinkFlavors :: Seq Text
drinkFlavors = empty
             |> "The cool water tantalizes"
             |> "The cool water refreshes"
             |> "The cool water invigorates"
             |> "Water is essential"
             |> "Fetch some more water?"

-- | Drink volumes
drinkVolumes :: Seq Milliliters
drinkVolumes = empty |> 25 |> 75 |> 150 |> 0 |> 0

mergeDrinkFlavors :: Seq Text -> Seq (Optional Text) -> Seq Text
mergeDrinkFlavors  = S.zipWith choose
  where
    choose x Default      = x
    choose _ (Specific y) = y

mergeOptionalSeq :: Seq a -> Seq (Optional a) -> Seq a
mergeOptionalSeq = S.zipWith choose
  where
    choose x Default      = x
    choose _ (Specific y) = y

-- | Override an Env with optionals.
overrideEnv :: Optional FilePath
            -> Optional FilePath
            -> Seq (Optional Text)
            -> Seq (Optional Milliliters)
            -> Optional Text
            -> Optional Text
            -> Env
            -> IO Env
overrideEnv a b c d e f env =
  let cooler    = toFilePath $ _cooler env
      history   = toFilePath $ _history env
      drinkText =  _drinkText env
      drinkVol  =  _drinkVolume env
      dt        = _timeFormat env
      thirstText= _thirstyText env
  in mkEnv (defaultTo cooler a)
           (defaultTo history b)
           (mergeOptionalSeq drinkText c)
           (mergeOptionalSeq drinkVol d)
           (defaultTo dt e)
           (defaultTo thirstText f)

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
          , _fakeDrinkVolume :: Seq Milliliters
          , _fakeTimeFormat  :: Text
          , _fakethirstyText :: Text
          } deriving (Show)

instance FromJSON FakeEnv where
  parseJSON (Object v) =
    FakeEnv <$> v .: "cooler"
            <*> v .: "history"
            <*> v .: "drinkText"
            <*> v .: "drinkVolume"
            <*> v .: "timeFormat"
            <*> v .: "thirstyText"
  parseJSON _ = mzero

instance ToJSON FakeEnv where
  toJSON (FakeEnv cooler history drinkText drinkVol timeFormat thirstyText) =
    object [ "cooler"      .= cooler
           , "history"     .= history
           , "drinkText"   .= drinkText
           , "drinkVolume" .= drinkVol
           , "timeFormat"  .= timeFormat
           , "thirstyText" .= thirstyText
           ]

-- | Create an Env from a FakeEnv
mkEnvFromFake :: MonadThrow m => FakeEnv -> m Env
mkEnvFromFake (FakeEnv a b c d e f) = mkEnv a b c d e f

-- | Create an Env to a FakeEnv
toFake :: Env -> FakeEnv
toFake (Env a b c d e f) = FakeEnv (toFilePath a) (toFilePath b) c d e f

-- | Read Env rc file
readEnvRC :: Path Abs File -> IO Env
readEnvRC file = join $
  unlessEmpty file (mkEnv' Default Default Default Default Default Default) $ \contents ->
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

-- | Get the drink volumesfrom the environment.
envGetDrinkVolumes :: Env -> Seq Milliliters
envGetDrinkVolumes = _drinkVolume

-- | Get the time format from the environment.
envGetTimeFormat :: Env -> Text
envGetTimeFormat = _timeFormat

-- | Get the thirsty text from the environment.
envGetThirstyText :: Env -> Text
envGetThirstyText = _thirstyText

-- | Run an action within a test environment.
-- The name of the test environment is specified as a string (e.g. "bench" or
-- "test", and simply controls the prefix of the filename used.
withTestEnv :: String -> (Env -> IO a) -> IO a
withTestEnv name f = do
  cooler  <- getTestFileName $ name ++ "FileCooler"
  history <- getTestFileName $ name ++ "nameFileHistory"
  env     <- mkEnv' (Specific cooler) (Specific history) Default Default Default Default

  result <- f env

  safeRemoveFile cooler
  safeRemoveFile history
  pure result

-- | Create a testing environment.
createTestEnv :: String -> IO Env
createTestEnv name = do
  cooler  <- getTestFileName $ name ++ "FileCooler"
  history <- getTestFileName $ name ++ "nameFileHistory"
  mkEnv' (Specific cooler) (Specific history) Default Default Default Default


-- | Destroy a testing environment.
destroyTestEnv :: Env -> IO ()
destroyTestEnv env = do
  safeRemoveFile $ toFilePath $ envGetCooler env
  safeRemoveFile $ toFilePath $ envGetHistory env

-- | Remove a file only if it exists.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile f = doesFileExist f >>= ifSo (removeFile f)
  where ifSo = flip when
