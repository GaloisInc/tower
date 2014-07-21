module Ivory.GRTOS.SearchDir where

import System.FilePath

import qualified Paths_grtos

searchDir :: IO FilePath
searchDir = do
  base <- Paths_grtos.getDataDir
  return $ base </> "support"

