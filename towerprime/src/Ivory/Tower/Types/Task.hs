
module Ivory.Tower.Types.Task
  ( Task(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.EventHandler

data Task =
  Task
    { task_evt_emitters :: [Chan]
    , task_evt_handlers :: [EventHandler]
    , task_moddef       :: ModuleDef
    }

