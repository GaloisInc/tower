module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , generatedCodeInsertModule
  , generatedCodeInsertThreadCode
  , generatedCodeInsertMonitorCode
  , emptyGeneratedCode
  ) where

import qualified Data.Map as Map
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.ToyObjLang
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.MonitorCode

data GeneratedCode = GeneratedCode
  { generatedcode_modules  :: [Module]
  , generatedcode_threads  :: Map.Map AST.Thread ThreadCode
  , generatedcode_monitors :: Map.Map AST.Monitor MonitorCode
  }

generatedCodeInsertModule :: Module
                          -> GeneratedCode -> GeneratedCode
generatedCodeInsertModule m g =
  g { generatedcode_modules = m : generatedcode_modules g }

generatedCodeInsertThreadCode :: ThreadCode
                              -> GeneratedCode -> GeneratedCode
generatedCodeInsertThreadCode tc g =
  g { generatedcode_threads = ins (generatedcode_threads g) }
  where ins = Map.insertWith addThreadCode (threadcode_thread tc) tc

generatedCodeInsertMonitorCode :: AST.Monitor -> MonitorCode
                               -> GeneratedCode -> GeneratedCode
generatedCodeInsertMonitorCode mast mc g =
  g { generatedcode_monitors = ins (generatedcode_monitors g) }
  where ins = Map.insertWith addMonitorCode mast mc

emptyGeneratedCode :: GeneratedCode
emptyGeneratedCode = GeneratedCode
  { generatedcode_modules  = []
  , generatedcode_threads  = Map.empty
  , generatedcode_monitors = Map.empty
  }

