module Ivory.Tower.AST.Signal where

import Ivory.Tower.Types.Time

data Signal = Signal
  -- Note: The Ord instance must sort first by deadline,
  -- otherwise interrupt handlers will not process
  -- interrupts in the correct order.
  { signal_deadline :: Microseconds
  , signal_name     :: String
  , signal_number   :: Int
  } deriving (Eq, Show, Ord)

