{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Backend where

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.SignalCode

import MonadLib

data SomeHandler backend = forall a. SomeHandler (TowerBackendHandler backend a)

class TowerBackend backend where
  -- XXX should probably be type families, not data families, and maybe at the
  -- top-level (without relying on the class).

  -- Type correponds to the channel type
  data TowerBackendCallback  backend :: Area * -> *
  data TowerBackendEmitter   backend :: *
  -- Type correponds to the channel type
  data TowerBackendHandler   backend :: Area * -> *
  data TowerBackendMonitor   backend :: *
  data TowerBackendOutput    backend :: *

  uniqueImpl   :: StateM m backend
               => Unique
               -> m String
  callbackImpl :: (StateM m backend, IvoryArea a)
               => -- Callback identifier, used to construct full callback name
                  Unique
               -- Implementation
               -> (forall s s'. ConstRef s' a -> Ivory (AllocEffects s) ())
               -> m (TowerBackendCallback backend a)
  emitterImpl :: (StateM m backend, IvoryArea b, IvoryZero b)
              => AST.Emitter
              -> [TowerBackendHandler backend b]
              -> m (Emitter b, TowerBackendEmitter backend)
  handlerImpl :: (StateM m backend, IvoryArea a, IvoryZero a)
              => AST.Handler
              -> [TowerBackendEmitter backend ]
              -> [TowerBackendCallback backend a]
              -> m (TowerBackendHandler backend a)
  monitorImpl :: StateM m backend
              => AST.Monitor
              -> [SomeHandler backend]
              -- Contains the state variable declarations for the monitor
              -> ModuleDef
              -> m (TowerBackendMonitor backend)
  towerImpl :: backend
            -> AST.Tower
            -> [TowerBackendMonitor backend]
            -> (backend, TowerBackendOutput backend)

-- | The result of running the Tower monad.
data TowerResult backend = TowerResult
  { tower_AST            :: AST.Tower
  , tower_backend        :: backend
  , tower_backend_output :: TowerBackendOutput backend
  , tower_depends        :: Dependencies
  , tower_signalCode     :: SignalCode
  }
