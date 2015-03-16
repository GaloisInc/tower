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
  , towerPutASTArtifact
  , towerCodegen
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Artifact

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
  fresh = towerCodegen fresh
  getEnv = towerCodegen getEnv


towerPutASTMonitor :: AST.Monitor -> Tower e ()
towerPutASTMonitor m = Tower $ sets_ $
  \s -> s { AST.tower_monitors = m : AST.tower_monitors s }

towerPutASTSyncChan :: AST.SyncChan -> Tower e ()
towerPutASTSyncChan a = Tower $ sets_ $
  \s -> s { AST.tower_syncchans = a : AST.tower_syncchans s }

towerPutASTPeriod :: AST.Period -> Tower e ()
towerPutASTPeriod a = Tower $ sets_ $ AST.towerInsertPeriod a

towerPutASTSignal :: AST.Signal -> Tower e ()
towerPutASTSignal a = Tower $ sets_ $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

towerCodegen :: Codegen e a -> Tower e a
towerCodegen x = Tower $ lift x

towerPutASTArtifact :: Artifact -> Tower e ()
towerPutASTArtifact a = Tower $ sets_ $
  \s -> s { AST.tower_artifact_fs = f : AST.tower_artifact_fs s }
  where f = artifactFileName a
