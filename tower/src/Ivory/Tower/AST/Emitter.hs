
module Ivory.Tower.AST.Emitter where

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.SyncChan
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

emitterProcName :: Emitter -> String
emitterProcName e = showUnique (emitter_name e)
  ++ case emitter_chan e of
       ChanSync c -> "_chan_" ++ show (sync_chan_label c)
       _ -> error ("impossible: emitterProcName invariant broken @ " ++ show e)
