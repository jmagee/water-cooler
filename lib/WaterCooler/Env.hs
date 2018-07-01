-- | Water cooler environment.

module WaterCooler.Env
( Env (..)
, mkEnv
) where

import           Control.Monad.Catch (MonadThrow)
import           Path                (Abs, File, Path, parseAbsFile)

data Env = Env { _cooler  :: Path Abs File
               , _history :: Path Abs File
               } deriving (Eq, Show)

-- | Create an Env.
mkEnv :: MonadThrow m => FilePath -> FilePath -> m Env
mkEnv a b = do
  cooler  <- parseAbsFile a
  history <- parseAbsFile b
  pure $ Env cooler history
