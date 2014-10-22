
module Ivory.Tower.Codegen.Thread
  ( generatedCodeModules
  ) where

import qualified Data.Map as Map

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

generatedCodeModules :: GeneratedCode -> AST.Tower -> [Module]
generatedCodeModules gc _XXX = generatedcode_modules gc ++
  map threadUserModule (Map.elems (generatedcode_threads gc)) ++
  map threadGenModule (Map.elems (generatedcode_threads gc))
  where
  threadUserModule t = package ("t_u_" ++ AST.threadName (threadcode_thread t))
                                (threadcode_user t)
  threadGenModule t = package ("t_g_" ++ AST.threadName (threadcode_thread t))
                               (threadcode_gen t)

