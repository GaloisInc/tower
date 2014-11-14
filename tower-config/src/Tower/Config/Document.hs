
module Tower.Config.Document where

import Tower.Config.Preprocess
import Tower.Config.TOML

getDocument :: FilePath -> [FilePath] -> IO (Either String TOML)
getDocument root path = do
  b <- getPreprocessedFile root path
  case b of
    Right bs -> case tomlParse bs of
      Just a -> return (Right a)
      Nothing -> return (Left "Error when TOML parsing config file")
    Left e -> return (Left e)


