
module Ivory.Tower.AST.Task
  ( Task(..)
  ) where

import Ivory.Language
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.EventHandler
import Ivory.Tower.AST.ChanReader

data Task =
  Task
    { task_evt_emitters :: [Chan]
    , task_evt_handlers :: [EventHandler]
    , task_chan_readers :: [ChanReader]
    , task_usercode     :: ModuleDef
    , task_commprims    :: ModuleDef
    }

