{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Emitter where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Unique
import Ivory.Language

data Emitter (a :: Area *) = Emitter AST.Emitter

emitterProcName :: Emitter a -> String
emitterProcName (Emitter e) = showUnique (AST.emitter_name e)
  ++ case AST.emitter_chan e of
    (AST.ChanSync (AST.SyncChan c)) -> "_chan_" ++ show c
    _ -> error ("impossible: emitterProcName invariant broken @ " ++ show e)
