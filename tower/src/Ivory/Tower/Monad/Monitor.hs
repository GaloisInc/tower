{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , monitorPutASTHandler
  , monitorModuleDef
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
                    (StateT MonitorCode (Tower e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor e ()
           -> Tower e (AST.Monitor, MonitorCode)
runMonitor n b = do
  u <- freshname n
  runStateT emptyMonitorCode
                    (fmap snd (runStateT (AST.emptyMonitor u) (unMonitor b)))

monitorPutASTHandler :: AST.Handler -> Monitor e ()
monitorPutASTHandler a = Monitor $ sets_ $
  \s -> s { AST.monitor_handlers = a : AST.monitor_handlers s }

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ lift $ lift $ a

monitorCodegen :: Codegen e a -> Monitor e a
monitorCodegen a = liftTower $ towerCodegen a

monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef def = Monitor $ lift $ sets_ $ insertMonitorCode def

monitorPutThreadCode :: (AST.Tower -> [ThreadCode]) -> Monitor e ()
monitorPutThreadCode = monitorCodegen . codegenThreadCode

instance BaseUtils Monitor e where
  fresh = liftTower fresh
  getEnv = liftTower getEnv
