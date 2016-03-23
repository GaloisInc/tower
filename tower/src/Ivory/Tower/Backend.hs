{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Backend where

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Emitter
import qualified Ivory.Language.Syntax.AST as IAST

data SomeHandler backend = SomeHandler (TowerBackendHandler backend)

class TowerBackend backend where
  -- XXX should probably be type families, not data families, and maybe at the
  -- top-level (without relying on the class).

  -- Type correponds to the channel type
  data TowerBackendCallback  backend :: *
  data TowerBackendEmitter   backend :: *
  -- Type correponds to the channel type
  data TowerBackendHandler   backend :: *
  data TowerBackendMonitor   backend :: *
  data TowerBackendOutput    backend :: *

  callbackImpl :: backend
               -- Callback identifier, used to construct full callback name
               -> String
               -- Implementation
               -> IAST.Proc
               -> TowerBackendCallback backend
  emitterImplAST :: (IvoryArea b, IvoryZero b)
                 => backend
                 -> AST.Emitter
                 -> Emitter b
  emitterImpl :: backend
              -> AST.Emitter
              -> [TowerBackendHandler backend]
              -> TowerBackendEmitter backend
  handlerImpl :: backend
              -> AST.Handler
              -> [TowerBackendEmitter backend]
              -> [TowerBackendCallback backend]
              -> TowerBackendHandler backend
  monitorImpl :: backend
              -> AST.Monitor
              -> [SomeHandler backend]
              -- Contains the state variable declarations for the monitor
              -> ModuleDef
              -> TowerBackendMonitor backend
  towerImpl :: backend
            -> AST.Tower
            -> [TowerBackendMonitor backend]
            -> TowerBackendOutput backend
