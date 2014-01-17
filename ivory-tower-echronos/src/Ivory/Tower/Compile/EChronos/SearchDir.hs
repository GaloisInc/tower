-- trevor taught me how to do this with TH if i promised not to commit it
-- and as much as i dont believe in commiting commented out code, i kinda
-- want this to work because its a neat (possibly unsound) trick. -pch
-- {-# LANGUAGE TemplateHaskell #-}

module Ivory.Tower.Compile.EChronos.SearchDir where

-- import Language.Haskell.TH (runIO)
-- import Language.Haskell.TH.Syntax (liftString)

import System.FilePath

import qualified Paths_ivory_tower_echronos

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_tower_echronos.getDataDir
  return $ base </> "ivory-echronos-wrapper"

-- searchDir :: FilePath
-- searchDir = base </> "ivory-echronos-wrapper"
--  where base = $( liftString =<< runIO Paths_ivory_tower_echronos.getDataDir )

