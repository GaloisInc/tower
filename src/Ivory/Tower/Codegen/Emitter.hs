{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Codegen.Emitter where

import Control.Monad (forM_)

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Codegen.Handler

import Ivory.Tower.ToyObjLang

emitterCode :: forall a
             . (IvoryArea a)
            => Emitter a -> AST.Tower -> AST.Thread -> EmitterCode a
emitterCode e twr thr = EmitterCode
  { emittercode_init = iproc
  , emittercode_emit = trampoline
  , emittercode_deliver = dproc
  , emittercode_user = do
      private $ incl trampoline -- XXX make sure this is private to c module.
  , emittercode_gen = do
      incl iproc
      incl eproc
      incl dproc
  }
  where
  tn = AST.threadName thr
  placeholder :: MemArea a
  placeholder = area (e_per_thread "storage_placeholder") Nothing
  trampoline :: Def('[ConstRef s a]:->())
  trampoline = proc ename $ \msg -> body $ call_ eproc msg
  iproc :: Def('[]:->())
  iproc = proc (e_per_thread "init") $ body $
               (comment ("XXX init in thread " ++ tn ))
  eproc :: Def('[ConstRef s a]:->())
  eproc = proc (e_per_thread "emit")  $ \_msg -> body $
               (comment ("XXX store messages for delivery"))
  dproc :: Def('[]:->())
  dproc = proc (e_per_thread "deliver") $ body $ do
               forM_ (AST.towerChanHandlers twr chanast) $ \(_,h) ->
                 call_ (handlerproc_stub h) (constRef (addrOf placeholder))
  handlerproc_stub :: AST.Handler -> Def('[ConstRef s a]:->())
  handlerproc_stub h = proc (handlerProcName h thr) $ \_msg -> body $
    return ()

  chanast = case e of Emitter (AST.Emitter _ ast _) -> ast
  ename = emitterProcName e
  e_per_thread suffix = ename ++ "_" ++ tn ++ "_" ++ suffix
