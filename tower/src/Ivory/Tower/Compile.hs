module Ivory.Tower.Compile
  ( towerCompile
  , TOpts(..)
  ) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import Ivory.Language
import Ivory.Artifact
import Ivory.Tower.Compile.Options
import System.Environment (getArgs)

towerCompile :: IO (TOpts, (C.Opts -> C.Opts) -> [Module] -> [Artifact] -> IO ())
towerCompile = do
  args <- getArgs
  (copts, topts) <- getOpts args
  let runc updateopts mods as =
        C.runCompilerWith Nothing mods as (updateopts copts)
  return (topts, runc)

