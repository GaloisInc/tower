{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Ivory.Language.Syntax as IAST hiding (Area)

import Ivory.Language
import Ivory.Language.Monad hiding (emit)
import Ivory.Language.Proc
import Ivory.Language.Area
import Ivory.Language.Type

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

callback :: forall a e. (IvoryArea a, IvoryZero a)
         => (forall s s' . ConstRef s a -> Ivory (AllocEffects s') ())
         -> Handler a e ()
callback b = handlerName >>= \ nm -> Handler $ do
  u <- freshname $ "callback_" ++ showUnique nm
  handlerPutASTCallback u
  backend <- handlerGetBackend
  handlerPutCodeCallback (callbackImpl backend u b) 
    $ IAST.Proc { IAST.procSym      = showUnique u
                , IAST.procRetTy    = IAST.TyVoid
                , IAST.procArgs     = [IAST.Typed (IAST.TyConstRef (ivoryArea (Proxy :: AProxy a))) var]
                , IAST.procBody     = blockStmts block
                , IAST.procRequires = blockRequires block
                , IAST.procEnsures  = blockEnsures block
                }
  where
    block = snd $ runIvory $ noReturn $ b arg
    (var,_) = genVar initialClosure -- initial closure is ok until we have one argument per function
    arg     = wrapVar var
    

callbackV :: (IvoryArea ('Stored a), IvoryStore a, IvoryZeroVal a)
          => (forall s' . a -> Ivory (AllocEffects s') ())
          -> Handler ('Stored a) e ()
callbackV b = callback (\bref -> deref bref >>= b)

emitV :: (IvoryArea ('Stored a), IvoryInit a, IvoryZeroVal a, GetAlloc eff ~ 'Scope s)
      => Emitter ('Stored a) -> a -> Ivory eff ()
emitV e v = do
  l <- local (ival v)
  emit e (constRef l)

