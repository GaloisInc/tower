
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , emptyMonitorCode
  , insertMonitorCode
  ) where

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

-- XXX CHANGE TO BE MODULEM () ONLY, PUT CODATA IN MONAD
data MonitorCode = MonitorCode
  { monitorcode_moddef :: AST.Monitor -> ModuleM ()
  }

emptyMonitorCode :: MonitorCode
emptyMonitorCode = MonitorCode
  { monitorcode_moddef = const (return ())
  }

insertMonitorCode :: (AST.Monitor -> ModuleM ())
                  -> MonitorCode -> MonitorCode
insertMonitorCode m c =
  c { monitorcode_moddef = \ctx -> monitorcode_moddef c ctx >> m ctx }

