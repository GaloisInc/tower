
module Ivory.Tower.Compile
  ( towerCompile
  , runTowerCompile
  , TOpts(..)
  ) where

import qualified Data.Map as Map
import Data.Monoid
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend.Compat
import Ivory.Tower.Compile.Options
import Ivory.Tower.Tower
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.SignalCode
import Ivory.Tower.Types.TowerPlatform

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
runTowerCodegen t p = generateTowerCode gc ast p
  where
  (ast, output, deps, sigs) = runTower CompatBackend t (platformEnv p)
  gc = GeneratedCode
    { generatedcode_modules = dependencies_modules deps
    , generatedcode_depends = dependencies_depends deps
    , generatedcode_threads = Map.insertWith mappend initThread mempty $ compatoutput_threads output
    , generatedcode_monitors = Map.map MonitorCode $ compatoutput_monitors output
    , generatedcode_signals = signalcode_signals sigs
    , generatedcode_init = signalcode_init sigs
    , generatedcode_artifacts = dependencies_artifacts deps
    }
  initThread = AST.InitThread AST.Init

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
  gcas = generatedcode_artifacts gc
  as = systemArtifacts p twr mods gcas ++ gcas
