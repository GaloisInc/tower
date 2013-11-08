
module Ivory.Tower.Compile.AADL.SearchDir where

import System.FilePath

import qualified Paths_ivory_tower_aadl

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_tower_aadl.getDataDir
  return $ base </> "ivory-aadl-headers"

