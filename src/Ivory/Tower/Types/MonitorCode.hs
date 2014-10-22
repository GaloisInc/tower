
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , emptyMonitorCode
  , insertMonitorCode
  , addMonitorCode
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

addMonitorCode :: MonitorCode -> MonitorCode -> MonitorCode
addMonitorCode a b = MonitorCode
  { monitorcode_moddef = monitorcode_moddef a >> monitorcode_moddef b
  }
