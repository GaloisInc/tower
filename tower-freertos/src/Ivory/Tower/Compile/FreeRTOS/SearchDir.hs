module Ivory.Tower.Compile.FreeRTOS.SearchDir where

import System.FilePath

import qualified Paths_towerprime_freertos

searchDir :: IO FilePath
searchDir = do
  base <- Paths_towerprime_freertos.getDataDir
  return $ base </> "ivory-freertos-wrapper"

