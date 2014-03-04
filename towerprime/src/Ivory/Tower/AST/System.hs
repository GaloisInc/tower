
module Ivory.Tower.AST.System
  ( System(..)
  ) where

import Ivory.Tower.AST.Chan
import qualified Ivory.Tower.AST.Directory as D
import Ivory.Tower.AST.Task
import Ivory.Tower.Types.Unique

data System =
  System
    { system_channels :: [Chan]
    , system_tasks    :: D.Dir Unique Task
    } deriving (Eq, Show)

