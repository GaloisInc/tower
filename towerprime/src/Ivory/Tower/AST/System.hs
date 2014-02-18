
module Ivory.Tower.AST.System
  ( System(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Directory
import Ivory.Tower.AST.Task
import Ivory.Tower.Types.Unique

data System =
  System
    { system_channels :: [Chan]
    , system_tasks    :: Dir Unique Task
    }

