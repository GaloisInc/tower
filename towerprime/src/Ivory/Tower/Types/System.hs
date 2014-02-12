
module Ivory.Tower.Types.System
  ( System(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.ATree
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Task
import Ivory.Tower.Types.Unique

data System =
  System 
    { system_channels :: [Chan]
    , system_tasks    :: ATree Unique Task
    , system_moddef   :: ModuleDef
    }

