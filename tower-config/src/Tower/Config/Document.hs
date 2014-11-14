
module Tower.Config.Document where

import Tower.Config.Preprocess

import qualified Text.TOML as T
import           Text.TOML.Value

getDocument :: FilePath -> [FilePath] -> IO (Either String Value)
getDocument root path = do
  b <- getPreprocessedFile root path
  case b of
    Right bs -> case tomlValue (T.parse bs) of
      Just a -> return (Right a)
      Nothing -> return (Left "Error when TOML parsing config file")
    Left e -> return (Left e)
  where
  tomlValue = fmap Left
