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


import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Base

import Ivory.Tower.Codegen.Emitter
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Language

emitter :: (IvoryArea a, IvoryZero a)
        => ChanInput a -> Integer -> Handler b e (Emitter a)
emitter (ChanInput (Chan chanast)) bound = do
  n <- fresh
  let ast = AST.emitter n chanast bound
      e = Emitter ast
  handlerPutASTEmitter ast
  handlerPutCodeEmitter $ emitterCode e
  return e

callback :: (IvoryArea a, IvoryZero a)
         => (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
         -> Handler a e ()
callback b = do
  u <- freshname "callback"
  handlerPutASTCallback u
  hname <- handlerName
  handlerPutCodeCallback $ \t -> do
    incl (callbackProc (callbackProcName u hname t) b)

callbackV :: (IvoryArea (Stored a), IvoryStore a, IvoryZeroVal a)
          => (forall s' . a -> Ivory (AllocEffects s') ())
          -> Handler (Stored a) e ()
callbackV b = callback (\bref -> deref bref >>= b)

callbackProc :: (IvoryArea a)
             => String
             -> (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
             -> Def('[ConstRef s a]:->())
callbackProc name f = proc name $ \m -> body $ noReturn $ f m

emit :: (IvoryArea a, IvoryZero a)
     => Emitter a -> ConstRef s a -> Ivory eff ()
emit e = call_ (callbackProc (emitterProcName e) (const (return ())))

emitV :: (IvoryArea (Stored a), IvoryInit a, IvoryZeroVal a, GetAlloc eff ~ Scope s)
      => Emitter (Stored a) -> a -> Ivory eff ()
emitV e v = do
  l <- local (ival v)
  emit e (constRef l)

