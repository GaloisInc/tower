
module Ivory.Tower.Compile
  ( towerCompile
  , runTowerCompile
  ) where

import Ivory.Tower.Tower
import Ivory.Tower.Codegen
import Ivory.Tower.Types.TowerPlatform

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C

towerCompile :: [TowerPlatform] -> Tower p () -> IO ()
towerCompile _platforms _t = do
  error "towerCompiler unimplemented"
  -- XXX get opts, determine TowerPlatform choice, parse remaining as C backend opts
  -- runTowerCompile

runTowerCodegen :: Tower p () -> TowerPlatform
                -> ([Module], [Artifact], [IO FilePath])
runTowerCodegen t p = (ms, as, searchpath)
  where
  (gc, ast) = runTower t
  (ms, as) = generateTowerCode ast gc p
  searchpath = platformSPath p

runTowerCompile :: Tower p () -> TowerPlatform -> C.Opts -> IO ()
runTowerCompile t p opts = do
  let (ms, as, spath) = runTowerCodegen t p
  _ <- C.runCompilerWith Nothing (Just spath) ms opts
  mapM_ (putArtifact (C.srcDir opts)) as

