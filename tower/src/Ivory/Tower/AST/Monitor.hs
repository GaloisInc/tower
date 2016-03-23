
module Ivory.Tower.AST.Monitor where

import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.Handler
import Ivory.Tower.Types.Opts
import Ivory.Language.Syntax

data Monitor = Monitor
  { monitor_name         :: Unique
  , monitor_handlers     :: [Handler]
  , monitor_external     :: MonitorExternal
  , monitor_module       :: Module
  , monitor_transformers :: [Opt]
  } deriving (Eq, Show, Ord)

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name

data MonitorExternal =
    MonitorDefined
  | MonitorExternal
  deriving (Show, Read, Eq, Ord)
