
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , emptyMonitorCode
  , insertMonitorCode
  , addMonitorCode
  ) where

import Ivory.Language

data MonitorCode = MonitorCode
  { monitorcode_moddef :: ModuleDef
  }

emptyMonitorCode :: MonitorCode
emptyMonitorCode = MonitorCode
  { monitorcode_moddef = return ()
  }

insertMonitorCode :: ModuleDef
                  -> MonitorCode -> MonitorCode
insertMonitorCode m c =
  c { monitorcode_moddef = monitorcode_moddef c >> m }

addMonitorCode :: MonitorCode -> MonitorCode -> MonitorCode
addMonitorCode a b = MonitorCode
  { monitorcode_moddef = monitorcode_moddef a >> monitorcode_moddef b
  }
