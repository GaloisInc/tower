
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
  } deriving (Eq, Show, Ord)

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name

data MonitorExternal =
    MonitorDefined
  | MonitorExternal
  deriving (Show, Read, Eq, Ord)
 


data MonitorFast = MonitorFast
  { monitor :: Monitor }

instance Ord MonitorFast where
  compare arg1 arg2 = 
    let a = monitor arg1 in
    let b = monitor arg2 in
    compare 
      (monitor_name a, map FastOpt $ monitor_transformers a) 
      (monitor_name b, map FastOpt $ monitor_transformers b) 

instance Eq MonitorFast where
  (==) arg1 arg2 =
    let a = monitor arg1 in
    let b = monitor arg2 in
    (==) 
      (monitor_name a, map FastOpt $ monitor_transformers a) 
      (monitor_name b, map FastOpt $ monitor_transformers b) 

instance Show MonitorFast where
  show arg1 =
    let a = monitor arg1 in
    show (monitor_name a, map FastOpt $ monitor_transformers a) 
