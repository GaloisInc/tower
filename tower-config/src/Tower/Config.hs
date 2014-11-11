
module Tower.Config
  ( module Tower.Config.Types
  , configFromFile
  ) where

import Tower.Config.Types
import Tower.Config.File

configFromFile :: (Configurable a)
               => FilePath -> [FilePath] -> IO (Either String a)
configFromFile root path = do
  f <- getConfigFile root path
  case f of
    Right bs -> case parse bs >>= fromConfig of
      Just a -> return (Right a)
      Nothing -> return (Left "Error parsing config file")
    Left e -> return (Left e)

