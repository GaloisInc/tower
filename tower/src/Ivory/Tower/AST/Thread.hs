module Ivory.Tower.AST.Thread
  ( Thread(..)
  , threadName
  , threadUserCodeModName
  , threadGenCodeModName
  , threadLoopProcName
  , threadChan
  ) where

import Ivory.Tower.Types.Time
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Chan

data Thread = SignalThread Signal
            | PeriodThread Period
            | InitThread   String -- initialization handler name
            deriving (Eq, Show)

threadName :: Thread -> String
threadName (SignalThread s) = "thread_signal_" ++ signal_name s
threadName (InitThread   s) = "thread_init_" ++ s
threadName (PeriodThread p) =
     "thread_period_"
  ++ prettyTime (period_dt p)
  ++ if toMicroseconds ph == 0
       then ""
       else "_phase_" ++ prettyTime ph
  where
  ph = period_phase p

threadUserCodeModName :: Thread -> String
threadUserCodeModName t = "tower_user_" ++ threadName t

threadGenCodeModName :: Thread -> String
threadGenCodeModName t = "tower_gen_" ++ threadName t

threadLoopProcName :: Thread -> String
threadLoopProcName t = "loop_" ++ threadName t

threadChan :: Thread -> Chan
threadChan (PeriodThread p) = ChanPeriod p
threadChan (SignalThread s) = ChanSignal s
threadChan (InitThread  _s) = ChanInit

instance Ord Thread where

  -- Init threads are greater than all other threads.
  compare (InitThread{}) (InitThread{})
    = EQ
  compare (InitThread{}) _
    = GT
  compare _              (InitThread{})
    = LT

  compare (PeriodThread p0) (PeriodThread p1)
    = compare (period_dt p0) (period_dt p1)
  -- Lexigraphical ordering on tie
  compare (SignalThread s0) (SignalThread s1)
    = let d0 = signal_deadline s0 in
      let d1 = signal_deadline s1 in
      if d0 == d1
        then compare (signal_name s0) (signal_name s1)
        else compare d0 d1

  compare (SignalThread s) (PeriodThread p)
    = breakTie (<=) (signal_deadline s) (period_dt p)
  compare (PeriodThread p) (SignalThread s)
    = breakTie (<) (period_dt p) (signal_deadline s)

breakTie :: (a -> a -> Bool)
         -> a -> a -> Ordering
breakTie op a b
  | a `op` b  = LT
  | otherwise = GT
