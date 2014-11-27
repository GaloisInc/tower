{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Base

import Ivory.Tower.Codegen.Emitter
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Language

emitter :: forall a b e
         . (IvoryArea a)
        => ChanInput a -> Integer -> Handler b e (Emitter a)
emitter (ChanInput (Chan chanast)) bound = do
  n <- fresh
  let ast = AST.emitter n chanast bound
      e = Emitter ast
  handlerPutASTEmitter ast
  handlerPutCodeEmitter $ \twr thr ->
    (emitterCode e twr thr :: EmitterCode a)
  return e

callback :: forall s a e
          . (IvoryArea a)
         => (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
         -> Handler a e ()
callback b = do
  u <- freshname "callback"
  handlerPutASTCallback u
  hname <- handlerName
  handlerPutCodeCallback $ \t -> do
    incl (callbackProc (callbackProcName u hname t) b)

callbackV :: forall a e
           . (IvoryArea (Stored a), IvoryVar a)
          => (forall s' . a -> Ivory (AllocEffects s') ())
          -> Handler (Stored a) e ()
callbackV b = callback (\bref -> deref bref >>= b)

callbackProc :: forall s a
              . (IvoryArea a)
             => String
             -> (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
             -> Def('[ConstRef s a]:->())
callbackProc name f = proc name $ \m -> body $ noReturn $ f m

emit :: forall eff s a
      . (IvoryArea a)
     => Emitter a -> ConstRef s a -> Ivory eff ()
emit e = call_ (callbackProc (emitterProcName e) (const (return ())))

emitV :: forall eff s a
       . (IvoryArea (Stored a), IvoryInit a, GetAlloc eff ~ Scope s)
      => Emitter (Stored a) -> a -> Ivory eff ()
emitV e v = do
  l <- local (ival v)
  emit e (constRef l)

