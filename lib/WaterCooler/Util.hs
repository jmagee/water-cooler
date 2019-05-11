-- | Utilities functions.
module WaterCooler.Util
( jbail
, defaultTo'
, maybeToOptional
, mkHomePath
, seqNubBy
, unlessEmpty
, slash
, writeJSON
, (<$$>)

-- Utilities for Testing
, getCWD
, getTestFileName
) where

import           Control.Monad            (liftM2)
import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString.Lazy     as BS (ByteString, null, readFile,
                                                 writeFile)
import           Data.Optional            (Optional (..), defaultTo)
import           Data.Sequence            as S (Seq (..), filter)
import           Path                     (Abs, Dir, File, Path, parseAbsDir,
                                           parseRelFile, toFilePath, (</>))
import           System.Directory         (doesFileExist, getCurrentDirectory,
                                           getHomeDirectory)
import           System.FilePath.Posix    (pathSeparator)

-- | Read the file, unless it is empty, in which case return a default value
unlessEmpty :: Path b File -> a -> (BS.ByteString -> a) -> IO a
unlessEmpty file def action =
  let f = toFilePath file
  in doesFileExist f >>= bool (pure def) (go <$> BS.readFile f)
  where
    go contents
      | BS.null contents = def
      | otherwise = action contents

-- | Insert a system specific path separator.
slash :: FilePath -> FilePath -> FilePath
slash a b = a ++ [pathSeparator] ++ b

-- | Create a path in the home directory.
mkHomePath :: FilePath -> IO FilePath
mkHomePath x = (`slash` x) <$> getHomeDirectory

-- | Bail out error when parsing a json file
jbail :: Path Abs File -> String -> a
jbail f e = error $ "Error parsing JSON file <" ++ toFilePath f ++ ">:" ++ e

-- | Write a json file.
writeJSON :: ToJSON a => Path Abs File -> a -> IO ()
writeJSON file thing = BS.writeFile (toFilePath file) $ encodePretty thing

-- | defaultTo that wraps the result back up in an Optional.
defaultTo' :: a -> Optional a -> Optional a
defaultTo' x y = Specific $ defaultTo x y

-- | Convert a Maybe into an Optional
maybeToOptional :: Maybe a -> Optional a
maybeToOptional (Just x) = Specific x
maybeToOptional Nothing  = Default

-- | Nested functor.
(<$$>) :: Functor f => Functor f' => (a -> b) -> f (f' a) -> f (f' b)
(<$$>) = fmap . fmap

-- | Get the current working directory.  Used for testing.
getCWD :: IO (Path Abs Dir)
getCWD = getCurrentDirectory >>= parseAbsDir

-- | Get the absolute name of a file to be used for testing.
-- This throws an error if the test file exists.
getTestFileName :: String -> IO FilePath
getTestFileName s = do
  f <- getFileName' s
  e <- doesFileExist $ toFilePath f
  if e
    then error $ "Test file exists, please manually remove: " ++ toFilePath f
    else pure $ toFilePath f
  where
    getFileName' = liftM2 (</>) getCWD . parseRelFile

-- | nubBy for Sequences.
seqNubBy :: (a -> a -> Bool) -> S.Seq a -> S.Seq a
seqNubBy _ S.Empty = S.Empty
seqNubBy f (x :<| xs) = x :<| seqNubBy f (nubFilter xs)
  where
    nubFilter = S.filter $ (not .) (f x)
