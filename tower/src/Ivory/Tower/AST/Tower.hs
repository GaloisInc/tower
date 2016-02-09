
module Ivory.Tower.AST.Tower
  ( Tower(..)
  , towerThreads
  , towerFindMonitorByName
  ) where

import Data.List (find, union)
import Data.Monoid
import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Monitor
import Ivory.Tower.AST.Thread
import Ivory.Tower.AST.Init

data Tower = Tower
  { tower_monitors    :: [Monitor]
  , tower_syncchans   :: [SyncChan]
  , tower_signals     :: [Signal]
  , tower_periods     :: [Period]
  } deriving (Eq, Show)

instance Monoid Tower where
  mempty = Tower
    { tower_monitors    = []
    , tower_syncchans   = []
    , tower_signals     = []
    , tower_periods     = []
    }
  mappend a b = Tower
    { tower_monitors    = tower_monitors    a `mappend` tower_monitors    b
    , tower_syncchans   = tower_syncchans   a `mappend` tower_syncchans   b
    , tower_signals     = tower_signals     a `mappend` tower_signals     b
    -- Periods are a set
    , tower_periods     = tower_periods     a `union`   tower_periods     b
    }

towerThreads :: Tower -> [Thread]
towerThreads t = map SignalThread (tower_signals t) ++
                 map PeriodThread (tower_periods t) ++
                 [ InitThread Init ]

towerFindMonitorByName :: Unique -> Tower -> Maybe Monitor
towerFindMonitorByName n t = find p (tower_monitors t)
  where p m = monitor_name m == n
