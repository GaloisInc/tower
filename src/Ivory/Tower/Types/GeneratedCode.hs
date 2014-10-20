module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , generatedCodeModules
  , insertTCGeneratedCode
  , emptyGeneratedCode
  ) where

import qualified Data.Map as Map
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.ToyObjLang
import Ivory.Tower.Types.ThreadCode

data GeneratedCode = GeneratedCode
  { generatedcode_modules :: [Module]
  , generatedcode_threads :: Map.Map AST.Thread ThreadCode
  }

generatedCodeModules :: GeneratedCode -> [Module]
generatedCodeModules gc = generatedcode_modules gc ++
  map threadUserModule (Map.elems (generatedcode_threads gc)) ++
  map threadGenModule (Map.elems (generatedcode_threads gc))
  where
  threadUserModule t = package ("t_u_" ++ AST.threadName (threadcode_thread t))                                (threadcode_user t)
  threadGenModule t = package ("t_g_" ++ AST.threadName (threadcode_thread t))
                               (threadcode_gen t)

insertTCGeneratedCode :: ThreadCode
                       -> GeneratedCode -> GeneratedCode
insertTCGeneratedCode tc g =
  g { generatedcode_threads = ins (generatedcode_threads g) }
  where ins = Map.insertWith addThreadCode (threadcode_thread tc) tc

emptyGeneratedCode :: GeneratedCode
emptyGeneratedCode = GeneratedCode [] Map.empty

