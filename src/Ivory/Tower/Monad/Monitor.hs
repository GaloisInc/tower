{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , monitorPutASTHandler
  , monitorPutModule
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

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

monitorPutASTHandler :: AST.Handler -> Monitor ()
monitorPutASTHandler a = withAST $
  \s -> s { AST.monitor_handlers = a : AST.monitor_handlers s }

monitorPutModule :: (AST.Monitor -> AST.Tower -> Module) -> Monitor ()
monitorPutModule m = Monitor $ do
  a <- get
  lift $ towerPutModule $
    \t -> m (findMonitorAST (AST.monitor_name a) t) t
  where
  findMonitorAST :: Unique -> AST.Tower -> AST.Monitor
  findMonitorAST n twr = maybe err id (AST.towerFindMonitorByName n twr)
  err = error "findMonitorAST failed - broken invariant"

instance BaseUtils Monitor where
  fresh = Monitor $ lift fresh
