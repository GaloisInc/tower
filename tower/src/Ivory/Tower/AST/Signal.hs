module Ivory.Tower.AST.Signal where

import Ivory.Tower.Types.Time

data Signal = Signal
  { signal_name     :: String
  , signal_deadline :: Microseconds
  } deriving (Eq, Show, Ord)

