
module Ivory.Tower.Compile.AADL.Assembly where

import Ivory.Language
import Ivory.Tower.Types

import qualified Ivory.Compile.AADL.AST as A
import qualified Ivory.Compile.AADL.Types as A

assemblyDoc :: String -> [Module] -> Assembly -> A.Document
assemblyDoc name mods asm = A.runCompile mods virtMod $ do
  return ()
  where
  virtMod = package name $ do
    mapM_ depend mods

