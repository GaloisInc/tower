
module Ivory.Tower.AST.Timer
  ( Timer(..)
  ) where

data Timer =
  Timer
    { timer_per :: Integer -- Microseconds
    } deriving (Eq, Show)

