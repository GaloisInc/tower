
module Ivory.Tower.Compile.AADL.SystemDoc
  ( systemDoc
  ) where

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Identifier
import Ivory.Compile.AADL.Monad
import Ivory.Compile.AADL.Gen (mkType, typeImpl)

import Ivory.Language
import qualified Ivory.Tower.AST as AST

systemDoc :: String -> [Module] -> AST.System p
          -> TypeCtxM (Document, [Warning])
systemDoc name ms _sysast = runCompile ms virtMod $ do
  return () -- XXX
  where
  virtMod = package name $ do
    mapM_ depend ms
