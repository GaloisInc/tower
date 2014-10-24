
module Ivory.Tower.AST.Chan
  ( Chan(..)
  ) where

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period

data Chan
  = ChanSync   SyncChan
  | ChanSignal Signal
  | ChanPeriod Period
  deriving (Eq, Show, Ord)
