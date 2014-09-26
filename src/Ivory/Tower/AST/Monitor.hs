
module Ivory.Tower.AST.Monitor where

import Ivory.Tower.Types.Unique

data Monitor = Monitor
  { monitor_name :: Unique
  } deriving (Eq, Show)

emptyMonitor :: Unique -> Monitor
emptyMonitor u = Monitor
  { monitor_name = u
  }
