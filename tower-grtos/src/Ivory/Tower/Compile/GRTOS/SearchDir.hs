module Ivory.Tower.Compile.GRTOS.SearchDir where

import System.FilePath

import qualified Paths_tower_grtos

searchDir :: IO FilePath
searchDir = do
  base <- Paths_tower_grtos.getDataDir
  return $ base </> "support"

