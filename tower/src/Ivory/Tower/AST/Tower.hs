
module Ivory.Tower.AST.Tower
  ( Tower(..)
  , emptyTower
  , towerThreads
  , towerInsertPeriod
  , towerFindMonitorByName
  , towerFindMonitorOfHandler
  ) where

import Data.List (find)
import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Monitor
import Ivory.Tower.AST.Thread
import Ivory.Tower.AST.Handler
import Ivory.Tower.AST.Init

data Tower = Tower
  { tower_monitors    :: [Monitor]
  , tower_syncchans   :: [SyncChan]
  , tower_signals     :: [Signal]
  , tower_periods     :: [Period]
  , tower_artifact_fs :: [String]
  } deriving (Eq, Show)

emptyTower :: Tower
emptyTower  = Tower
  { tower_monitors    = []
  , tower_syncchans   = []
  , tower_signals     = []
  , tower_periods     = []
  , tower_artifact_fs = []
  }

towerThreads :: Tower -> [Thread]
towerThreads t = map SignalThread (tower_signals t) ++
                 map PeriodThread (tower_periods t) ++
                 [ InitThread Init ]

-- Periods are a set
towerInsertPeriod :: Period -> Tower -> Tower
towerInsertPeriod p t
  | p `elem` (tower_periods t) = t
  | otherwise = t { tower_periods = p : tower_periods t }

towerFindMonitorByName :: Unique -> Tower -> Maybe Monitor
towerFindMonitorByName n t = find p (tower_monitors t)
  where p m = monitor_name m == n

towerFindMonitorOfHandler :: Handler -> Tower -> Maybe Monitor
towerFindMonitorOfHandler h t = find p (tower_monitors t)
  where p m = h `elem` monitor_handlers m
