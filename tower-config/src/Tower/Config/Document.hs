
module Tower.Config.Document where

import Tower.Config.Types
import Tower.Config.Preprocess

getDocument :: FilePath -> [FilePath] -> IO (Either String Value)
getDocument root path = do
  b <- getPreprocessedFile root path
  case b of
    Right bs -> case parse bs of
      Just a -> return (Right a)
      Nothing -> return (Left "Error parsing config file")
    Left e -> return (Left e)

