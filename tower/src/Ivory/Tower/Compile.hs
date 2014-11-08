
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

towerCompile :: (IO (TowerPlatform e)) -> Tower e () -> IO ()
towerCompile _getPlatform _t = do
  error "towerCompiler unimplemented"
  -- XXX get opts, get TowerPlatform, parse remaining as C backend opts
  -- runTowerCompile

runTowerCodegen :: Tower e () -> TowerPlatform e
                -> ([Module], [Artifact])
runTowerCodegen t p = generateTowerCode ast gc p
  where (gc, ast) = runTower t (platformEnv p)

runTowerCompile :: Tower e () -> TowerPlatform e -> C.Opts -> IO ()
runTowerCompile t p opts = do
  let (ms, as) = runTowerCodegen t p
  C.runCompilerWith Nothing ms as opts

generateTowerCode :: GeneratedCode -> AST.Tower -> TowerPlatform e
                  -> ([Module], [Artifact])
generateTowerCode gc twr p = (ms, as)
  where
  ms = generatedcode_modules gc
    ++ threadModules p gc twr
    ++ monitorModules p gc twr
    ++ systemModules p twr
  as = systemArtifacts p twr ms
