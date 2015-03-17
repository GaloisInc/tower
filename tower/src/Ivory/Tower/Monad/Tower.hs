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
import Data.Monoid
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Artifact

import Ivory.Tower.Types.GeneratedCode

import qualified Ivory.Tower.AST as AST

newtype Tower e a = Tower
  { unTower :: WriterT AST.Tower (Codegen e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower e () -> e -> (AST.Tower, GeneratedCode)
runTower t e = (a,b)
  where
  (a,b) = runBase e (runCodegen outer a)
  outer = fmap snd (runWriterT (unTower t))

instance BaseUtils Tower e where
  fresh = towerCodegen fresh
  getEnv = towerCodegen getEnv

towerPutASTMonitor :: AST.Monitor -> Tower e ()
towerPutASTMonitor m = Tower $ put $ mempty { AST.tower_monitors = [m] }

towerPutASTSyncChan :: AST.SyncChan -> Tower e ()
towerPutASTSyncChan a = Tower $ put $ mempty { AST.tower_syncchans = [a] }

towerPutASTPeriod :: AST.Period -> Tower e ()
towerPutASTPeriod a = Tower $ put $ mempty { AST.tower_periods = [a] }

towerPutASTSignal :: AST.Signal -> Tower e ()
towerPutASTSignal a = Tower $ put $ mempty { AST.tower_signals = [a] }

towerCodegen :: Codegen e a -> Tower e a
towerCodegen x = Tower $ lift x

towerPutASTArtifact :: Artifact -> Tower e ()
towerPutASTArtifact a = Tower $ put $
  mempty { AST.tower_artifact_fs = [artifactFileName a] }
