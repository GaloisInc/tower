
module Ivory.Tower.Compile
  ( towerCompile
  , runTowerCompile
  , TOpts(..)
  ) where

import Ivory.Tower.Tower
import Ivory.Tower.Types.GeneratedCode
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.TowerPlatform
import Ivory.Tower.Compile.Options

import Ivory.Language
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C

import System.Environment (getArgs)

towerCompile :: (TOpts -> IO (TowerPlatform e)) -> Tower e () -> IO ()
towerCompile mkPlatform t = do
  args <- getArgs
  (copts, topts) <- getOpts args
  p <- mkPlatform topts
  runTowerCompile t p copts

runTowerCodegen :: Tower e () -> TowerPlatform e
                -> ([Module], [Artifact])
runTowerCodegen t p = generateTowerCode ast gc p
  where (gc, ast) = runTower t (platformEnv p)

runTowerCompile :: Tower e () -> TowerPlatform e -> C.Opts -> IO ()
runTowerCompile t p opts = do
  let (mods, as) = runTowerCodegen t p
  C.runCompilerWith Nothing mods as opts

generateTowerCode :: GeneratedCode -> AST.Tower -> TowerPlatform e
                  -> ([Module], [Artifact])
generateTowerCode gc twr p = (mods, as)
  where
  mods = generatedcode_modules gc
      ++ threadModules p gc twr
      ++ monitorModules p gc twr
      ++ systemModules p twr
  as = systemArtifacts p twr mods
