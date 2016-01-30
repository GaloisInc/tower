module Ivory.Tower.AST.Signal where

import Ivory.Tower.Types.Time

data Signal = Signal
  { signal_name     :: String
  , signal_deadline :: Microseconds
  , signal_number   :: Int
  } deriving (Eq, Show, Ord)

