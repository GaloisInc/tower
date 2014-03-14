
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
  | ExternEvt String
  deriving (Eq, Show)

eventName :: Event -> String
eventName (ChanEvt c _) = "chan" ++ (show (chan_id c))
eventName (TimerEvt t)  = "timer" ++ (show (timer_id t))
                       ++ "_per" ++ (show (timer_per t))
eventName (ExternEvt e) = "extern_" ++ e

