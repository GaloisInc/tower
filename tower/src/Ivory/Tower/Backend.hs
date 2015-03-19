{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Backend where

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Unique

class TowerBackend backend where
  data TowerBackendCallback backend :: Area * -> *
  data TowerBackendEmitter backend :: *
  data TowerBackendHandler backend :: Area * -> *

  callbackImpl :: IvoryArea a => backend -> Unique -> (forall s s'. ConstRef s' a -> Ivory (AllocEffects s) ()) -> TowerBackendCallback backend a
  emitterImpl :: (IvoryArea b, IvoryZero b) => backend -> AST.Emitter -> (Emitter b, TowerBackendEmitter backend)
  handlerImpl :: (IvoryArea a, IvoryZero a) => backend -> AST.Handler -> [TowerBackendEmitter backend] -> [TowerBackendCallback backend a] -> TowerBackendHandler backend a
