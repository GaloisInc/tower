
module Ivory.Tower.AST.Emitter where

import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Unique

data Emitter = Emitter
  { emitter_name :: Unique
  , emitter_chan :: Chan
  , emitter_bound :: Integer
  } deriving (Eq, Show, Ord)

emitter :: Integer -> Chan -> Integer -> Emitter
emitter i c b = Emitter
  { emitter_name  = Unique "emitter" i
  , emitter_chan  = c
  , emitter_bound = b
  }
