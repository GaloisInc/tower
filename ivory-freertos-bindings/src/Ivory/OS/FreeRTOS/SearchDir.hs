module Ivory.OS.FreeRTOS.SearchDir where

import System.FilePath

import qualified Paths_ivory_freertos_bindings

searchDir :: IO FilePath
searchDir = do
    base <- Paths_ivory_freertos_bindings.getDataDir
    return $ base </> "ivory-freertos-wrapper"

