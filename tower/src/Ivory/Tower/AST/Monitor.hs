
module Ivory.Tower.AST.Monitor where

import Data.List (find)
import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.Handler

data Monitor = Monitor
  { monitor_name     :: Unique
  , monitor_handlers :: [Handler]
  } deriving (Eq, Show, Ord)

emptyMonitor :: Unique -> Monitor
emptyMonitor u = Monitor
  { monitor_name     = u
  , monitor_handlers = []
  }

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name

monitorFindHandlerByName :: Unique -> Monitor -> Maybe Handler
monitorFindHandlerByName n m = find p (monitor_handlers m)
  where p h = handler_name h == n

