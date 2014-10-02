module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , generatedCodeModules
  , generatedCodeForThread
  , emptyGeneratedCode
  ) where

import qualified Data.Map as Map
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.ToyObjLang

data GeneratedCode = GeneratedCode
  { generatedcode_modules :: [Module]
  , generatedcode_threads :: Map.Map AST.Thread (ModuleM ())
  }

generatedCodeModules :: GeneratedCode -> [Module]
generatedCodeModules gc = generatedcode_modules gc ++
  map threadModule (Map.toList (generatedcode_threads gc))
  where
  threadModule (t, moddef) =
    package ("tower_" ++ AST.threadName t) moddef

generatedCodeForThread :: AST.Thread -> ModuleM ()
                       -> GeneratedCode -> GeneratedCode
generatedCodeForThread t m g =
  g { generatedcode_threads = ins (generatedcode_threads g) }
  where ins = Map.insertWith (>>) t m

emptyGeneratedCode :: GeneratedCode
emptyGeneratedCode = GeneratedCode [] Map.empty

