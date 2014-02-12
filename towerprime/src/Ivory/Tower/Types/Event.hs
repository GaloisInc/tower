
module Ivory.Tower.Types.Event
  ( Event(..)
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Timer

data Event
  = ChanEvt   Chan
  | TimerEvt  Timer
  | ExternEvt String

