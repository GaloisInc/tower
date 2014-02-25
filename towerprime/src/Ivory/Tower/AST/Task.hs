
module Ivory.Tower.AST.Task
  ( Task(..)
  ) where

import Ivory.Tower.AST.ChanEmitter
import Ivory.Tower.AST.ChanReceiver
import Ivory.Tower.AST.EventHandler

data Task =
  Task
    { task_chan_emitters  :: [ChanEmitter]
    , task_chan_receivers :: [ChanReceiver]
    , task_evt_handlers   :: [EventHandler]
    , task_priority       :: Integer
    }

