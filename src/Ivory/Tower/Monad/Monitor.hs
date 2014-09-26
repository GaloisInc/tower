{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , putASTHandler
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

newtype Monitor a = Monitor
  { unMonitor :: StateT AST.Monitor Tower a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor () -> Tower AST.Monitor
runMonitor n b = do
  f <- fresh
  let m = AST.emptyMonitor (Unique n f)
  fmap snd (runStateT m (unMonitor b))

withAST :: (AST.Monitor -> AST.Monitor) -> Monitor ()
withAST f = Monitor $ do
  a <- get
  set (f a)

putASTHandler :: AST.Handler -> Monitor ()
putASTHandler a = withAST $
  \s -> s { AST.monitor_handlers = a : AST.monitor_handlers s }

instance BaseUtils Monitor where
  fresh = Monitor $ lift fresh
