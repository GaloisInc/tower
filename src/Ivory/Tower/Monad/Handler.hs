{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , runHandler
  , handlerName
  , handlerPutASTEmitter
  , handlerPutASTCallback
  , handlerPutCodeEmitter
  , handlerPutCodeCallback
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Handler a = Handler
  { unHandler :: StateT AST.Handler
                  (StateT [(AST.Thread, HandlerCode)] Monitor) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: String -> AST.Chan -> Handler ()
           -> Monitor ()
runHandler n ch b = mdo
  u <- freshname n
  towerast <- monitorGetGeneratedAST
  let h = AST.emptyHandler u ch
      ehcs = zip (AST.handlerThreads towerast handlerast)
                 (repeat emptyHandlerCode)
  (handlerast, tcs) <- runStateT ehcs $ fmap snd (runStateT h (unHandler b))

  monitorPutASTHandler handlerast
  monitorPutThreadCode (const [ (t, generateHandlerCode c) | (t,c) <- tcs ])

withAST :: (AST.Handler -> AST.Handler) -> Handler ()
withAST f = Handler $ do
  a <- get
  set (f a)

handlerName :: Handler Unique
handlerName = Handler $ do
  a <- get
  return (AST.handler_name a)

handlerPutASTEmitter :: AST.Emitter -> Handler ()
handlerPutASTEmitter a = withAST (AST.handlerInsertEmitter a)

handlerPutASTCallback :: Unique -> Handler ()
handlerPutASTCallback a = withAST (AST.handlerInsertCallback a)

withCode :: (AST.Thread -> HandlerCode -> HandlerCode) -> Handler ()
withCode f = Handler $ do
  a <- lift get
  lift (set [(t, f t c) | (t, c) <- a ])

handlerPutCodeCallback :: (AST.Thread -> ModuleM ()) -> Handler ()
handlerPutCodeCallback ms = withCode $ \t -> insertHandlerCodeCallback (ms t)

handlerPutCodeEmitter :: (AST.Thread -> EmitterCode) -> Handler ()
handlerPutCodeEmitter ms = withCode $ \t -> insertHandlerCodeEmitter (ms t)

instance BaseUtils Handler where
  fresh = Handler $ lift $ lift fresh
