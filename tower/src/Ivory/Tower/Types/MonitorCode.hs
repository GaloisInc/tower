
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  , addMonitorCode
  ) where

import Ivory.Language

data MonitorCode = MonitorCode
  { monitorcode_moddef :: ModuleDef
  }

addMonitorCode :: MonitorCode -> MonitorCode -> MonitorCode
addMonitorCode a b = MonitorCode
  { monitorcode_moddef = monitorcode_moddef a >> monitorcode_moddef b
  }
