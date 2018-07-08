-- | Utilities functions.
module WaterCooler.Util
( jbail
, defaultTo'
, mkHomePath
, unlessEmpty
, slash
, writeJSON
) where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString.Lazy     as BS (ByteString, null, readFile,
                                                 writeFile)
import           Data.Optional            (Optional (..), defaultTo)
import           Path                     (Abs, File, Path, toFilePath)
import           System.Directory         (doesFileExist, getHomeDirectory)
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

-- defaultTo that wraps the result back up in an Optional.
defaultTo' :: a -> Optional a -> Optional a
defaultTo' x y = Specific $ defaultTo x y
