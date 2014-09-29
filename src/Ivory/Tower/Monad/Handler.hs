{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , runHandler
  , putASTEmitter
  , putASTCallback
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import qualified Ivory.Tower.AST as AST

newtype Handler a = Handler
  { unHandler :: StateT AST.Handler Monitor a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: String -> AST.Chan -> Handler () -> Monitor AST.Handler
runHandler n c b = do
  f <- fresh
  let h = AST.emptyHandler (Unique n f) c
  fmap snd (runStateT h (unHandler b))

withAST :: (AST.Handler -> AST.Handler) -> Handler ()
withAST f = Handler $ do
  a <- get
  set (f a)

putASTEmitter :: AST.Emitter -> Handler ()
putASTEmitter a = withAST (AST.handlerInsertEmitter a)

putASTCallback :: String -> Handler ()
putASTCallback a = withAST (AST.handlerInsertCallback a)

instance BaseUtils Handler where
  fresh = Handler $ lift fresh
