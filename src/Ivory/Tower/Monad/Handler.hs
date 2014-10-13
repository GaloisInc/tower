{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , runHandler
  , handlerPutASTEmitter
  , handlerPutASTCallback
  , handlerPutCode
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Handler a = Handler
  { unHandler :: StateT AST.Handler (StateT HandlerCode Monitor) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: String -> AST.Chan -> Handler ()
           -> Monitor (AST.Handler, HandlerCode)
runHandler n c b = do
  u <- freshname n
  let h = AST.emptyHandler u c
  runStateT emptyHandlerCode (fmap snd (runStateT h (unHandler b)))

withAST :: (AST.Handler -> AST.Handler) -> Handler ()
withAST f = Handler $ do
  a <- get
  set (f a)

handlerPutASTEmitter :: AST.Emitter -> Handler ()
handlerPutASTEmitter a = withAST (AST.handlerInsertEmitter a)

handlerPutASTCallback :: String -> Handler ()
handlerPutASTCallback a = withAST (AST.handlerInsertCallback a)

withCode :: (HandlerCode -> HandlerCode) -> Handler ()
withCode f = Handler $ do
  a <- lift get
  lift (set (f a))

handlerPutCode :: (AST.Handler -> AST.Thread -> ModuleM ())
                 -> Handler ()
handlerPutCode ms = withCode $ insertHandlerCode ms

instance BaseUtils Handler where
  fresh = Handler $ lift $ lift fresh
