{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Tower
  ( Tower(..)
  , Tower'
  , runTower
  , towerGetBackend
  , towerPutMonitor
  , towerPutASTSyncChan
  , towerPutASTSignal
  , towerPutASTPeriod
  , towerCodegen
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid
import Ivory.Tower.Backend
import Ivory.Tower.Backend.Compat
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Types.GeneratedCode

import qualified Ivory.Tower.AST as AST

newtype Tower e a = Tower
  { unTower :: forall backend. TowerBackend backend => Tower' backend e a
  }
-- GHC can't derive these trivial instances because of the RankNType.

instance Functor (Tower e) where
  fmap f (Tower h) = Tower $ fmap f h

instance Monad (Tower e) where
  return x = Tower $ return x
  Tower x >>= f = Tower $ x >>= (unTower . f)

instance Applicative (Tower e) where
  pure = return
  (<*>) = ap

instance MonadFix (Tower e) where
  mfix f = Tower $ mfix (unTower . f)

newtype Tower' backend e a = Tower'
  { unTower' :: ReaderT backend (WriterT (AST.Tower, [TowerBackendMonitor backend]) (Codegen e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower e () -> e -> (AST.Tower, GeneratedCode)
runTower t e = (a, output `mappend` b)
  where
  CompatOutput output = towerImpl CompatBackend a monitors
  ((a, monitors), b) = runBase e (runCodegen outer a)
  outer = fmap snd (runWriterT (runReaderT CompatBackend (unTower' (unTower t))))

instance BaseUtils (Tower' backend) e where
  fresh = Tower' $ lift $ lift fresh
  getEnv = Tower' $ lift $ lift getEnv

instance BaseUtils Tower e where
  fresh = Tower fresh
  getEnv = Tower getEnv

towerGetBackend :: Tower' backend e backend
towerGetBackend = Tower' ask

towerPutMonitor :: AST.Monitor -> TowerBackendMonitor backend -> Tower' backend e ()
towerPutMonitor ast m = Tower' $ put (mempty { AST.tower_monitors = [ast] }, [m])

towerPutASTSyncChan :: AST.SyncChan -> Tower e ()
towerPutASTSyncChan a = Tower $ Tower' $ put (mempty { AST.tower_syncchans = [a] }, mempty)

towerPutASTPeriod :: AST.Period -> Tower e ()
towerPutASTPeriod a = Tower $ Tower' $ put (mempty { AST.tower_periods = [a] }, mempty)

towerPutASTSignal :: AST.Signal -> Tower e ()
towerPutASTSignal a = Tower $ Tower' $ put (mempty { AST.tower_signals = [a] }, mempty)

towerCodegen :: Codegen e a -> Tower e a
towerCodegen x = Tower $ Tower' $ lift $ lift x
