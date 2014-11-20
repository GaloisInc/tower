
module Ivory.Tower.AST.Thread
  ( Thread(..)
  , threadName
  , threadChan
  , threadDeadline
  ) where

import Ivory.Tower.Types.Time
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Init

data Thread = SignalThread Signal
            | PeriodThread Period
            | InitThread   Init
            deriving (Eq, Show)

threadName :: Thread -> String
threadName (SignalThread s) = "thread_signal_" ++ signal_name s
threadName (PeriodThread p) = "thread_period_" ++ prettyTime (period_dt p)
threadName (InitThread _)   = "thread_init"


threadChan :: Thread -> Chan
threadChan (PeriodThread p) = ChanPeriod p
threadChan (SignalThread s) = ChanSignal s
threadChan (InitThread   i) = ChanInit   i

threadDeadline :: Thread -> Microseconds
threadDeadline (PeriodThread p) = period_dt p
threadDeadline (SignalThread s) = signal_deadline s
threadDeadline (InitThread _)   = Microseconds 100000000 -- hack

instance Ord Thread where
  compare a b
    | threadDeadline a == threadDeadline b = tiebreak a b
    | otherwise = compare (threadDeadline a) (threadDeadline b)

tiebreak :: Thread -> Thread -> Ordering
-- initthread always greatest deadline
tiebreak (InitThread _) (InitThread _) = EQ
tiebreak (InitThread _) _              = GT
tiebreak _              (InitThread _) = LT
-- Break tie between signal and period - period is greater
tiebreak (SignalThread _)  (PeriodThread _) = LT
tiebreak (PeriodThread _) (SignalThread _) = GT
-- Break tie between signals - based on signal name
tiebreak (SignalThread s0) (SignalThread s1) =
  compare (signal_name s0) (signal_name s1)
-- Periods are a set, should not need to break ties.
tiebreak (PeriodThread _) (PeriodThread _) = EQ
