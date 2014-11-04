
module Ivory.Tower.Compile
  ( towerCompile
  , runTowerCompile
  ) where

import Ivory.Tower.Tower
import Ivory.Tower.Types.GeneratedCode
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.TowerPlatform

import Ivory.Language
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C

towerCompile :: [TowerPlatform] -> Tower p () -> IO ()
towerCompile _platforms _t = do
  error "towerCompiler unimplemented"
  -- XXX get opts, determine TowerPlatform choice, parse remaining as C backend opts
  -- runTowerCompile

runTowerCodegen :: Tower p () -> TowerPlatform
                -> ([Module], [Artifact])
runTowerCodegen t p = generateTowerCode ast gc p
  where (gc, ast) = runTower t

runTowerCompile :: Tower p () -> TowerPlatform -> C.Opts -> IO ()
runTowerCompile t p opts = do
  let (ms, as) = runTowerCodegen t p
  C.runCompilerWith Nothing ms as opts
--  mapM_ (putArtifact (C.srcDir opts)) as

generateTowerCode :: GeneratedCode -> AST.Tower -> TowerPlatform
                  -> ([Module], [Artifact])
generateTowerCode gc twr p = (ms, as)
  where
  ms = generatedcode_modules gc
    ++ threadModules p gc twr
    ++ monitorModules p gc twr
    ++ systemModules p twr
  as = systemArtifacts p twr ms
