module Ivory.Tower.AST.Period where

import Ivory.Tower.Types.Time

data Period = Period
  { period_dt :: Microseconds
  } deriving (Eq, Show, Ord)

