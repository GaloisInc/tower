module Ivory.Tower.Compile
  ( towerCompile
  , TOpts(..)
  ) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import Ivory.Language
import Ivory.Artifact
import Ivory.Tower.Compile.Options
import System.Environment (getArgs)

towerCompile :: IO (TOpts, [Module] -> [Artifact] -> IO ())
towerCompile = do
  args <- getArgs
  (copts, topts) <- getOpts args
  return (topts, \mods as -> C.runCompilerWith Nothing mods as copts)

