
module Ivory.Tower.AST.Event
  ( Event(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Timer

data Event
  = ChanEvt   Chan
  | TimerEvt  Timer
  | ExternEvt String

