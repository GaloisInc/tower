
module Ivory.Tower.AST.Thread where

import Ivory.Tower.Types.Time
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Chan

data Thread = SignalThread Signal
            | PeriodThread Period
            deriving (Eq, Show, Ord)

threadName :: Thread -> String
threadName (SignalThread s) = "thread_signal_" ++ signal_name s
threadName (PeriodThread p) = "thread_period_" ++ t
  where
  us = toMicroseconds (period_dt p)
  t = case us `mod` 1000 of
    0 -> (show (us `div` 1000)) ++ "ms"
    _ -> (show us) ++ "us"


threadChan :: Thread -> Chan
threadChan (PeriodThread p) = ChanPeriod p
threadChan (SignalThread s) = ChanSignal s
