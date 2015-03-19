{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Backend.Compat where

import Control.Arrow (second)
import Data.Monoid
import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Codegen.Emitter
import Ivory.Tower.Codegen.Handler
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.ThreadCode

data CompatBackend = CompatBackend

instance TowerBackend CompatBackend where
  newtype TowerBackendCallback CompatBackend a = CompatCallback (forall s. AST.Handler -> AST.Thread -> (Def ('[ConstRef s a] :-> ()), ModuleDef))
  newtype TowerBackendEmitter CompatBackend = CompatEmitter (AST.Tower -> AST.Thread -> SomeEmitterCode)
  newtype TowerBackendHandler CompatBackend a = CompatHandler (AST.Tower -> [(AST.Thread, ThreadCode)])

  callbackImpl _ ast f = CompatCallback $ \ h -> callbackCode ast (AST.handler_name h) f

  emitterImpl _ ast =
    let (e, code) = emitterCode ast
    in (e, CompatEmitter $ \ twr thd -> SomeEmitterCode $ code twr thd)

  handlerImpl _ ast emitters callbacks = CompatHandler $ \ twr -> generateHandlerThreadCode hc twr ast
    where
    hc = HandlerCode
      { handlercode_callbacks = \ t -> second mconcat $ unzip [ c ast t | CompatCallback c <- callbacks ]
      , handlercode_emitters = \ twr t -> [ e twr t | CompatEmitter e <- emitters ]
      }
