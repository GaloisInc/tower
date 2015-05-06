{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Handler
  ( emitter
  , Emitter()
  , callback
  , callbackV
  , emit
  , emitV
  , Handler()
  ) where

import Ivory.Tower.Backend
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Base

import qualified Ivory.Tower.AST as AST

import Ivory.Language

emitter :: (IvoryArea a, IvoryZero a)
        => ChanInput a -> Integer -> Handler b e (Emitter a)
emitter (ChanInput chan@(Chan chanast)) bound = handlerName >>= \ nm -> Handler $ do
  -- the only things that produce ChanInput will only put ChanSync in it.
  let AST.ChanSync syncchan = chanast
  n <- freshname $ "emitter_" ++ showUnique nm ++ "_chan_" ++ show (AST.sync_chan_label syncchan)
  let ast = AST.emitter n syncchan bound
  handlerPutASTEmitter ast
  backend <- handlerGetBackend
  handlers <- handlerGetHandlers chan
  let (e, code) = emitterImpl backend ast handlers
  handlerPutCodeEmitter code
  return e

callback :: (IvoryArea a, IvoryZero a)
         => (forall s s' . ConstRef s a -> Ivory (AllocEffects s') ())
         -> Handler a e ()
callback b = handlerName >>= \ nm -> Handler $ do
  u <- freshname $ "callback_" ++ showUnique nm
  handlerPutASTCallback u
  backend <- handlerGetBackend
  handlerPutCodeCallback $ callbackImpl backend u b

callbackV :: (IvoryArea (Stored a), IvoryStore a, IvoryZeroVal a)
          => (forall s' . a -> Ivory (AllocEffects s') ())
          -> Handler (Stored a) e ()
callbackV b = callback (\bref -> deref bref >>= b)

emitV :: (IvoryArea (Stored a), IvoryInit a, IvoryZeroVal a, GetAlloc eff ~ Scope s)
      => Emitter (Stored a) -> a -> Ivory eff ()
emitV e v = do
  l <- local (ival v)
  emit e (constRef l)

