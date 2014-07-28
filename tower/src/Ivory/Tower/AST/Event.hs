
module Ivory.Tower.AST.Event
  ( Event(..)
  , eventName
  ) where

import Ivory.Tower.AST.ChanReceiver
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Timer

data Event
  = ChanEvt   Chan ChanReceiver
  | TimerEvt  Timer
  | SignalEvt String
  deriving (Eq, Show)

eventName :: Event -> String
eventName (ChanEvt c _) = "chan_" ++ (show (chan_id c))
eventName (TimerEvt t)  = "timer_" ++ (show (timer_per t))
eventName (SignalEvt n) = "signal_" ++ n

