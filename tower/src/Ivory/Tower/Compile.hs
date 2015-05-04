module Ivory.Tower.Compile
  ( towerCompile
  , runTowerCompile
  , TOpts(..)
  ) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import Ivory.Tower.Backend
import Ivory.Tower.Compile.Options
import Ivory.Tower.Tower
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.TowerPlatform
import System.Environment (getArgs)

towerCompile :: TowerBackend backend
             => (TOpts -> IO (TowerPlatform backend e))
             -> Tower e ()
             -> IO ()
towerCompile mkPlatform t = do
  args <- getArgs
  (copts, topts) <- getOpts args
  p <- mkPlatform topts
  runTowerCompile t p copts

runTowerCompile :: TowerBackend backend
                => Tower e ()
                -> TowerPlatform backend e
                -> C.Opts
                -> IO ()
runTowerCompile t p opts = C.runCompilerWith Nothing mods as opts
  where
--  (_ast, _backend, output, deps, sigs) = runTower (platformBackend p) t (platformEnv p)
  res = runTower (platformBackend p) t (platformEnv p)
  deps = tower_depends res
  sigs = tower_signalCode res
  output = tower_backend_output res
  mods = dependencies_modules deps ++ addModules p output deps sigs
  givenArtifacts = dependencies_artifacts deps
  as = addArtifacts p output deps sigs mods givenArtifacts ++ givenArtifacts
