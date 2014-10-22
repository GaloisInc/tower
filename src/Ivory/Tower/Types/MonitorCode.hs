
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , emptyMonitorCode
  , insertMonitorCode
  ) where

import Ivory.Tower.ToyObjLang

data MonitorCode = MonitorCode
  { monitorcode_moddef :: ModuleM ()
  }

emptyMonitorCode :: MonitorCode
emptyMonitorCode = MonitorCode
  { monitorcode_moddef = return ()
  }

insertMonitorCode :: ModuleM ()
                  -> MonitorCode -> MonitorCode
insertMonitorCode m c =
  c { monitorcode_moddef = monitorcode_moddef c >> m }

