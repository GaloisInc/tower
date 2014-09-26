
module Ivory.Tower.AST.Emitter where

import Ivory.Tower.AST.Chan

data Emitter = Emitter
  { emitter_chan :: Chan
  , emitter_bound :: Integer
  } deriving (Eq, Show, Ord)

