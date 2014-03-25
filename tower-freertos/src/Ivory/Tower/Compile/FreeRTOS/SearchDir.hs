module Ivory.Tower.Compile.FreeRTOS.SearchDir where

import System.FilePath

import qualified Paths_tower_freertos

searchDir :: IO FilePath
searchDir = do
  base <- Paths_tower_freertos.getDataDir
  return $ base </> "ivory-freertos-wrapper"

