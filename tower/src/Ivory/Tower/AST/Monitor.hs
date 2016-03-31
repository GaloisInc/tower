
module Ivory.Tower.AST.Monitor where

import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.Handler
import Ivory.Tower.Types.Opts
import qualified Ivory.Language.Syntax as I

data Monitor = Monitor
  { monitor_name         :: Unique
  , monitor_handlers     :: [Handler]
  , monitor_external     :: MonitorExternal
  , monitor_moduledef    :: I.Module
  , monitor_transformers :: [Opt]
  } deriving (Eq, Show)

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name

instance Ord Monitor where
  compare a b = compare 
    (monitor_name a, monitor_handlers a, monitor_external a, monitor_transformers a) 
    (monitor_name b, monitor_handlers b, monitor_external b, monitor_transformers b) 

data MonitorExternal =
    MonitorDefined
  | MonitorExternal
  deriving (Show, Read, Eq, Ord)
 