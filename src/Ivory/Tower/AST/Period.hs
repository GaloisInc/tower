module Ivory.Tower.AST.Period where

import Ivory.Tower.Types.Time

data Period = Period Microseconds deriving (Eq, Show, Ord)

