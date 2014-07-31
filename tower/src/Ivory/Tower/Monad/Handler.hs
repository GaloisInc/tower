{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , runHandler
  , putHandlerEmitter
  , putHandlerModuleDef
  , putHandlerSetupCode
  , putHandlerCallback
  , putHandlerFinalizerCode
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative (Applicative)

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Monad.Task
import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.HandlerContext
import Ivory.Tower.Types.Unique

newtype Handler p (area :: Area *) a = Handler
  { unHandler :: StateT AST.Handler (HandlerCodegen p area) a
  } deriving (Functor, Monad, Applicative, MonadFix)


newtype HandlerCodegen p (area :: Area *) a = HandlerCodegen
  { unHandlerCodegen :: StateT (HandlerContext -> HandlerCode area) (Task p) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: forall p area
            . Handler p area ()
           -> Unique
           -> String
           -> AST.Trigger
           -> Task p (AST.Handler, (HandlerContext -> HandlerCode area))
runHandler h n a t = do
  ((_,ast),c) <- runHandlerCodegen $ runStateT emptyast (unHandler h)
  return (ast, c)
  where
  runHandlerCodegen g = runStateT (\_ -> emptycode) (unHandlerCodegen g)
  emptycode :: HandlerCode area
  emptycode = HandlerCode
    { handlercode_handlername = n
    , handlercode_moddef      = return ()
    , handlercode_setup       = return ()
    , handlercode_callback    = \_ -> return ()
    , handlercode_finalizer   = return ()
    }
  emptyast :: AST.Handler
  emptyast = AST.Handler
    { AST.handler_name       = n
    , AST.handler_annotation = a
    , AST.handler_trigger    = t
    , AST.handler_emitters   = []
    }

instance BaseUtils (Handler p area) where
  getOS = handlerLiftTask getOS
  fresh = handlerLiftTask fresh

handlerLiftTask :: Task p a
                -> Handler p area a
handlerLiftTask t = Handler $ lift $ HandlerCodegen $ lift t

-- Internal API to AST

getAST :: Handler p area AST.Handler
getAST = Handler get

setAST :: AST.Handler -> Handler p area ()
setAST a = Handler (set a)

-- External API to AST

putHandlerEmitter :: AST.ChanEmitter -> Handler p area ()
putHandlerEmitter e = do
  a <- getAST
  setAST $ a { AST.handler_emitters = e : (AST.handler_emitters a) }

-- Internal API to HandlerCode

getCode :: Handler p area (HandlerContext -> HandlerCode area)
getCode = Handler $ lift $ HandlerCodegen get

setCode :: (HandlerContext -> HandlerCode area) -> Handler p area ()
setCode c = Handler $ lift $ HandlerCodegen $ set c

-- External API to HandlerCode

putHandlerModuleDef :: (HandlerContext -> ModuleDef) -> Handler p area ()
putHandlerModuleDef m = do
  c <- getCode
  setCode $ \ctx -> (c ctx)
    { handlercode_moddef = handlercode_moddef (c ctx) >> (m ctx) }

putHandlerSetupCode :: (forall s . HandlerContext -> Ivory (AllocEffects s) ())
                    -> Handler p area ()
putHandlerSetupCode m = do
  c <- getCode
  setCode $ \ctx -> (c ctx)
    { handlercode_setup = handlercode_setup (c ctx) >> (m ctx) }

putHandlerCallback :: (forall s s' . ConstRef s area
                                  -> Ivory (AllocEffects s') ())
                   -> Handler p area ()
putHandlerCallback cb = do
  c <- getCode
  setCode $ \ctx -> (c ctx)
    { handlercode_callback = \r -> handlercode_callback (c ctx) r >> cb r }

putHandlerFinalizerCode :: (forall s . HandlerContext
                                    -> Ivory (AllocEffects s) ())
                        -> Handler p area ()
putHandlerFinalizerCode m = do
  c <- getCode
  setCode $ \ctx -> (c ctx)
    { handlercode_finalizer = handlercode_finalizer (c ctx) >> (m ctx) }

