module Ivory.Tower.AST.Signal where

data Signal = Signal
  { signal_name :: String
  } deriving (Eq, Show, Ord)

