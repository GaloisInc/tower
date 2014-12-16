
module Ivory.Tower.AST.Chan
  ( Chan(..)
  ) where

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Init

data Chan
  = ChanSync   SyncChan
  | ChanSignal Signal
  | ChanPeriod Period
  | ChanInit   Init
  deriving (Eq, Show, Ord)
