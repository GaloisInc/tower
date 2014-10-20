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

import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Handler a = Handler
  { unHandler :: StateT AST.Handler
                  (StateT (AST.Tower -> [(AST.Thread, HandlerCode)]) Monitor) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: String -> AST.Chan -> Handler ()
           -> Monitor ()
runHandler n ch b = mdo
  u <- freshname n
  let h = AST.emptyHandler u ch
      ehcs towerast = [ (t, emptyHandlerCode)
                      | t <- AST.handlerThreads towerast handlerast
                      ]
  (handlerast, thcs) <- runStateT ehcs $ fmap snd (runStateT h (unHandler b))

  monitorPutASTHandler handlerast
  monitorPutThreadCode $ \twr -> [ handlerCodeToThreadCode t hc
                                 | (t, hc) <- thcs twr
                                 ]

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

withCode :: (AST.Tower -> AST.Thread -> HandlerCode -> HandlerCode)
         -> Handler ()
withCode f = Handler $ do
  tcs <- lift get
  lift (set (\twr -> [(t, f twr t c) | (t, c) <- tcs twr ]))

handlerPutCodeCallback :: (AST.Thread -> ModuleM ()) -> Handler ()
handlerPutCodeCallback ms = withCode $ \_ t -> insertHandlerCodeCallback (ms t)

handlerPutCodeEmitter :: (AST.Tower -> AST.Thread -> EmitterCode) -> Handler ()
handlerPutCodeEmitter ms = withCode $ \a t -> insertHandlerCodeEmitter (ms a t)

instance BaseUtils Handler where
  fresh = Handler $ lift $ lift fresh
