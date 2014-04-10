
module Ivory.Tower.AST.Timer
  ( Timer(..)
  ) where

data Timer =
  Timer
    { timer_id    :: Integer
    , timer_per   :: Integer -- Microseconds
    } deriving (Eq, Show)

