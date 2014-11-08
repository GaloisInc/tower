{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Handler
  ( emitter
  , callback
  , emit
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
         => (forall eff . ConstRef s a -> Ivory eff ()) -> Handler a e ()
callback b = do
  u <- freshname "callback"
  handlerPutASTCallback u
  hname <- handlerName
  handlerPutCodeCallback $ \t -> do
    incl (callbackProc (callbackProcName u hname t) b)


callbackProc :: forall s a
              . (IvoryArea a)
             => String
             -> (forall eff . ConstRef s a -> Ivory eff ())
             -> Def('[ConstRef s a]:->())
callbackProc name f = proc name $ \m -> body $ f m

emit :: forall eff s a
      . (IvoryArea a)
     => Emitter a -> ConstRef s a -> Ivory eff ()
emit e = call_ (callbackProc (emitterProcName e) (const (return ())))

