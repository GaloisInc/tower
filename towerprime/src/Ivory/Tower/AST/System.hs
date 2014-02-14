
module Ivory.Tower.AST.System
  ( System(..)
  ) where

import Ivory.Language
import Ivory.Tower.AST.ATree
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Task
import Ivory.Tower.Types.Unique

data System =
  System
    { system_channels :: [Chan]
    , system_tasks    :: ATree Unique Task
    , system_commprim :: ModuleDef
    }

