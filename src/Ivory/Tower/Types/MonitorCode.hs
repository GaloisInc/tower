
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , emptyMonitorCode
  , insertMonitorCode
  , generateMonitorCode
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Unique

import Ivory.Tower.ToyObjLang

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

generateMonitorCode :: MonitorCode
                    -> AST.Monitor
                    -> [Module]
generateMonitorCode mc mon = [package n (monitorcode_moddef mc mon)]
  where n = "tower_monitor_" ++ showUnique (AST.monitor_name mon)

