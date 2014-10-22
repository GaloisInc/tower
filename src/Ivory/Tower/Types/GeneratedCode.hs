module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
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

insertTCGeneratedCode :: ThreadCode
                       -> GeneratedCode -> GeneratedCode
insertTCGeneratedCode tc g =
  g { generatedcode_threads = ins (generatedcode_threads g) }
  where ins = Map.insertWith addThreadCode (threadcode_thread tc) tc

emptyGeneratedCode :: GeneratedCode
emptyGeneratedCode = GeneratedCode [] Map.empty

