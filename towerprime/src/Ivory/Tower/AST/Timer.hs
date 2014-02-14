
module Ivory.Tower.AST.Timer
  ( Timer(..)
  ) where

data Timer =
  Timer
    { timer_id    :: Integer
    , timer_per   :: Integer -- Microseconds
    , timer_phase :: Integer
    } deriving (Eq, Show)

