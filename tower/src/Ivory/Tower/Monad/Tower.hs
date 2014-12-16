{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Tower
  ( Tower
  , runTower
  , towerPutASTMonitor
  , towerPutASTSyncChan
  , towerPutASTSignal
  , towerPutASTPeriod
  , towerCodegen
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen

import Ivory.Tower.Types.GeneratedCode

import qualified Ivory.Tower.AST as AST

newtype Tower e a = Tower
  { unTower :: StateT AST.Tower (Codegen e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower e () -> e -> (AST.Tower, GeneratedCode)
runTower t e = (a,b)
  where
  (a,b) = runBase e (runCodegen outer a)
  outer = fmap snd (runStateT AST.emptyTower (unTower t))

instance BaseUtils Tower e where
  fresh = Tower $ lift fresh
  getEnv = Tower $ lift getEnv

withAST :: (AST.Tower -> AST.Tower) -> Tower e ()
withAST f = Tower $ do
  a <- get
  set (f a)

towerPutASTMonitor :: AST.Monitor -> Tower e ()
towerPutASTMonitor m = withAST $
  \s -> s { AST.tower_monitors = m : AST.tower_monitors s }

towerPutASTSyncChan :: AST.SyncChan -> Tower e ()
towerPutASTSyncChan a = withAST $
  \s -> s { AST.tower_syncchans = a : AST.tower_syncchans s }

towerPutASTPeriod :: AST.Period -> Tower e ()
towerPutASTPeriod a = withAST $ AST.towerInsertPeriod a

towerPutASTSignal :: AST.Signal -> Tower e ()
towerPutASTSignal a = withAST $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

towerCodegen :: Codegen e a -> Tower e a
towerCodegen = Tower . lift

