{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Tower
  ( Tower
  , runTower
  , putASTMonitor
  , putASTSyncChan
  , putASTSignal
  , putASTPeriod
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Ivory.Tower.Monad.Base

import qualified Ivory.Tower.AST as AST

newtype Tower a = Tower
  { unTower :: StateT AST.Tower Base a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower () -> AST.Tower
runTower b = snd (runBase (runStateT AST.emptyTower (unTower b)))

instance BaseUtils Tower where
  fresh = Tower $ lift fresh

withAST :: (AST.Tower -> AST.Tower) -> Tower ()
withAST f = Tower $ do
  a <- get
  set (f a)

putASTMonitor :: AST.Monitor -> Tower ()
putASTMonitor m = withAST $
  \s -> s { AST.tower_monitors = m : AST.tower_monitors s }

putASTSyncChan :: AST.SyncChan -> Tower ()
putASTSyncChan a = withAST $
  \s -> s { AST.tower_syncchans = a : AST.tower_syncchans s }

putASTPeriod :: AST.Period -> Tower ()
putASTPeriod a = withAST $
  \s -> s { AST.tower_periods = a : AST.tower_periods s }

putASTSignal :: AST.Signal -> Tower ()
putASTSignal a = withAST $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

