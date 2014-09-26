
module Ivory.Tower.Types.Chan where

import qualified Ivory.Tower.AST as AST

data Chan a
  = SyncChan   AST.SyncChan
  | SignalChan AST.Signal
  | PeriodChan AST.Period
  deriving (Eq, Show)

newtype ChanInput a = ChanInput (Chan a)
newtype ChanOutput a = ChanOutput (Chan a)

