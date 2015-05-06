
module Ivory.Tower.AST.Emitter where

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.Types.Unique

data Emitter = Emitter
  { emitter_name :: Unique
  , emitter_chan :: SyncChan
  , emitter_bound :: Integer
  } deriving (Eq, Show, Ord)

emitter :: Unique -> SyncChan -> Integer -> Emitter
emitter i c b = Emitter
  { emitter_name  = i
  , emitter_chan  = c
  , emitter_bound = b
  }
