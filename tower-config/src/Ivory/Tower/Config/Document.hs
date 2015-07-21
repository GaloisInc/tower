
module Ivory.Tower.Config.Document where

import Ivory.Tower.Config.Preprocess
import Ivory.Tower.Config.TOML

getDocument :: FilePath -> [FilePath] -> IO (Either String TOML)
getDocument root path = do
  b <- getPreprocessedFile root path
  case b of
    Right bs -> return (tomlParse bs)
    Left e -> return (Left e)


