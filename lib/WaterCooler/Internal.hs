-- | Internal buildings blocks for water cooler.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Internal
( DrinkSize (..)
, Drink (..)
, WaterCooler (..)
, archiveHistory
, drink
, now
, magicTimeThreshold
, nextDrink
, readHistory
, readWaterCooler
, writeWaterCooler
) where

import           WaterCooler.FromString

import           Control.Monad             (mzero)
import           Data.Aeson                (FromJSON, ToJSON, Value (..),
                                            eitherDecode, object, parseJSON,
                                            toJSON, (.:), (.=))
import           Data.Aeson.Encode.Pretty  (encodePretty)
import           Data.Bool                 (bool)
import qualified Data.ByteString.Lazy      as BS (ByteString, null, readFile,
                                                  writeFile)
import           Data.Char                 (toLower)
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, diffUTCTime)
import           Data.Time.Clock           (getCurrentTime)
import           Path                      (Abs, File, Path, toFilePath)
import           System.Directory          (doesFileExist)
import           Test.QuickCheck           (oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

-- | Magic time threshold.
-- This threshold is used when comparing times.  Two times with a difference
-- within this range are considered to be effectively "the same".  This is
-- primarily used for tests.
magicTimeThreshold :: NominalDiffTime
magicTimeThreshold = 60

-- | Drink size.
-- Sip, Swallow, or Gulp- roughly small, medium, or large.
data DrinkSize = Sip | Swallow | Gulp
               deriving (Eq, Read, Show)

instance FromString DrinkSize where
  fromString s = case toLower <$> s of
    "sip"     -> Just Sip
    "swallow" -> Just Swallow
    "gulp"    -> Just Gulp
    _         -> Nothing

instance Arbitrary DrinkSize where
  arbitrary = oneof [pure Gulp, pure Swallow, pure Sip]
  shrink Gulp = [Swallow, Sip]
  shrink Swallow = [Sip]
  shrink Sip = []

-- | A drink - when and how much.
data Drink =
  Drink { _howMuch :: DrinkSize
        , _when    :: UTCTime
        } deriving (Show)

instance Eq Drink where
  (Drink a b) == (Drink c d) = (a == c) && (diffUTCTime b d <= magicTimeThreshold)

instance FromJSON Drink where
  -- parseJSON (Object v) = Drink <$> v .: "when" <*> (read <$> (v .: "howMuch"))
  parseJSON (Object v) = Drink <$> (read <$> (v .: "howMuch")) <*> (read <$> (v .: "when"))
  parseJSON _ = mzero

instance ToJSON Drink where
  toJSON (Drink howMuch when) =
    object [ "howMuch"    .= show howMuch
           , "when"       .= show when
           ]

-- | The keeper of fluids.
data WaterCooler =
  WaterCooler { _lastDrink      :: Drink
              , _secondsToNext  :: NominalDiffTime
              -- , _drinks         :: [Drink]
              -- Probably want to maintain drink list separately.
              } deriving (Show)

instance FromJSON WaterCooler where
  parseJSON (Object v) =
    WaterCooler <$> v .: "lastDrink"
                <*> v .: "secondsToNext"
  parseJSON _ = mzero

instance ToJSON WaterCooler where
  toJSON (WaterCooler lastDrink secondsToNext) =
    object [ "lastDrink"      .= lastDrink
           , "secondsToNext"  .= secondsToNext
           ]

-- | Drink some water now.
drink :: DrinkSize -> IO Drink
drink size = Drink size <$> now

-- | Get the current time.
now :: IO UTCTime
now = {-utcToLocalTime <$> getCurrentTimeZone <*> -}getCurrentTime

-- | Write a json file.
writeJSON :: ToJSON a => Path Abs File -> a -> IO ()
writeJSON file thing = BS.writeFile (toFilePath file) $ encodePretty thing

-- | Write the water cooler file.
writeWaterCooler :: Path Abs File -> WaterCooler -> IO ()
writeWaterCooler = writeJSON

-- | Bail out error when parsing a json file
jbail :: Path Abs File -> String -> a
jbail f e = error $ "Error parsing JSON file <" ++ toFilePath f ++ ">:" ++ e

-- | Read the water cooler file.
readWaterCooler :: Path Abs File -> IO (Maybe WaterCooler)
readWaterCooler file = unlessEmpty file Nothing $ \contents ->
  either (jbail file) Just (eitherDecode contents :: Either String WaterCooler)

-- | Read history.
readHistory :: Path Abs File -> IO [Drink]
readHistory file = unlessEmpty file [] $ \contents ->
  either (jbail file) id (eitherDecode contents :: Either String [Drink])

-- | Archive the water cooler history.
archiveHistory :: Path Abs File -> Path Abs File -> IO ()
archiveHistory coolFile histFile = readWaterCooler coolFile >>= \case
  Nothing                        -> pure ()
  Just (WaterCooler lastDrink _) -> do
    history <- readHistory histFile
    seq history $ writeJSON histFile $ lastDrink : history

-- | The time of the next drink
nextDrink :: WaterCooler -> UTCTime
nextDrink (WaterCooler (Drink _ lastd) next) = addUTCTime next lastd

-- | Read the file, unless it is empty, in which case return a default value
unlessEmpty :: Path b File -> a -> (BS.ByteString -> a) -> IO a
unlessEmpty file def action =
  let f = toFilePath file
  in doesFileExist f >>= bool (pure def) (go <$> BS.readFile f)
  where
    go contents
      | BS.null contents = def
      | otherwise = action contents
