-- | Internal buildings blocks for water cooler.
{-# LANGUAGE OverloadedStrings #-}

module WaterCooler.Internal
( DrinkSize (..)
, Drink (..)
, WaterCooler (..)
, drink
, now
, nextDrink
, readWaterCooler
, writeWaterCooler
) where

import           Control.Monad            (mzero)
import           Data.Aeson               (FromJSON, ToJSON, Value (..),
                                           eitherDecode, object, parseJSON,
                                           toJSON, (.:), (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString.Lazy     as BS (ByteString (..), null,
                                                 readFile, writeFile)
import           Data.Time                (LocalTime, UTCTime, NominalDiffTime,
                                           defaultTimeLocale,
                                           getCurrentTimeZone, parseTimeOrError,
                                           utcToLocalTime, addUTCTime)
import           Data.Time.Clock          (getCurrentTime)
import           Path                     (Abs, File, Path, toFilePath)
import           System.Directory         (doesFileExist)

-- | Drink size.
-- Sip, Swallow, or Gulp- roughly small, medium, or large.
data DrinkSize = Sip | Swallow | Gulp
               deriving (Read, Show)

-- | A drink - when and how much.
data Drink =
  Drink { _howMuch :: DrinkSize
        , _when    :: UTCTime
        } deriving (Show)

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

-- | Write the water cooler file.
writeWaterCooler :: Path Abs File -> WaterCooler -> IO ()
writeWaterCooler file wc = BS.writeFile (toFilePath file) $ encodePretty wc

-- | Read the water cooler file.
readWaterCooler :: Path Abs File -> IO (Maybe WaterCooler)
readWaterCooler file = unlessEmpty file Nothing $ \contents ->
  either bail Just (eitherDecode contents :: Either String WaterCooler)
  where
    bail err =  error $ "Error parsing JSON file <" ++ toFilePath file
                                                    ++ ">:" ++ err

-- | The time of the next drink
nextDrink :: WaterCooler -> UTCTime
nextDrink (WaterCooler (Drink _ last) next) = addUTCTime next last

-- | Read the file, unless it is empty, in which case return a default value
unlessEmpty :: Path b File -> a -> (BS.ByteString -> a) -> IO a
unlessEmpty file def action =
  let f = toFilePath file
  in doesFileExist f >>= bool (pure def) (go <$> BS.readFile f)
  where
    go contents
      | BS.null contents = def
      | otherwise = action contents

-- | Convert a string into LocalTime.
-- We define this instead of using read, because has a friendlier parse.
-- For example, read :: LocalTime, would choke on "2016-1-2", but the solution
-- below parses it as "2016-01-02".
dateFromString :: String -> LocalTime
dateFromString = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"
