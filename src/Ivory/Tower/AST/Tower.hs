
module Ivory.Tower.AST.Tower where

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Monitor

data Tower = Tower
  { tower_monitors  :: [Monitor]
  , tower_syncchans :: [SyncChan]
  , tower_signals   :: [Signal]
  , tower_periods   :: [Period]
  } deriving (Eq, Show)

emptyTower :: Tower
emptyTower  = Tower
  { tower_monitors  = []
  , tower_syncchans = []
  , tower_signals   = []
  , tower_periods   = []
  }

