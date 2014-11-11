{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , monitorPutASTHandler
  , monitorPutCode
  , monitorPutThreadCode
  , liftTower -- XXX UNSAFE TO USE
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

import Ivory.Language

newtype Monitor e a = Monitor
  { unMonitor :: StateT AST.Monitor
                    (StateT (AST.Monitor -> MonitorCode) (Tower e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor e ()
           -> Tower e (AST.Monitor, MonitorCode)
runMonitor n b = do
  u <- freshname n
  (ast, mkmc) <- runStateT (const emptyMonitorCode)
                    (fmap snd (runStateT (AST.emptyMonitor u) (unMonitor b)))
  return (ast, mkmc ast)

withAST :: (AST.Monitor -> AST.Monitor) -> Monitor e ()
withAST f = Monitor $ do
  a <- get
  set (f a)

monitorPutASTHandler :: AST.Handler -> Monitor e ()
monitorPutASTHandler a = withAST $
  \s -> s { AST.monitor_handlers = a : AST.monitor_handlers s }

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ lift $ lift $ a

monitorCodegen :: Codegen e a -> Monitor e a
monitorCodegen = liftTower . towerCodegen

withCode :: (AST.Monitor -> MonitorCode -> MonitorCode) -> Monitor e ()
withCode f = Monitor $ do
  a <- lift get
  lift (set (\ctx -> (f ctx (a ctx))))

monitorPutCode :: (AST.Monitor -> ModuleDef) -> Monitor e ()
monitorPutCode f = withCode $ \ctx mc -> insertMonitorCode (f ctx) mc

monitorPutThreadCode :: (AST.Tower -> [ThreadCode]) -> Monitor e ()
monitorPutThreadCode = monitorCodegen . codegenThreadCode

instance BaseUtils Monitor e where
  fresh = liftTower fresh
  getEnv = liftTower getEnv
