
module Ivory.Tower.AST.Monitor where

import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.Handler

data Monitor = Monitor
  { monitor_name     :: Unique
  , monitor_handlers :: [Handler]
  } deriving (Eq, Show, Ord)

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name
