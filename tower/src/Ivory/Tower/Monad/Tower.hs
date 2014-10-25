{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype Tower a = Tower
  { unTower :: StateT AST.Tower Codegen a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower () -> (AST.Tower, GeneratedCode)
runTower t = (a,b)
  where
  (a,b) = runBase (runCodegen outer a)
  outer = fmap snd (runStateT AST.emptyTower (unTower t))

instance BaseUtils Tower where
  fresh = Tower $ lift fresh

withAST :: (AST.Tower -> AST.Tower) -> Tower ()
withAST f = Tower $ do
  a <- get
  set (f a)

towerPutASTMonitor :: AST.Monitor -> Tower ()
towerPutASTMonitor m = withAST $
  \s -> s { AST.tower_monitors = m : AST.tower_monitors s }

towerPutASTSyncChan :: AST.SyncChan -> Tower ()
towerPutASTSyncChan a = withAST $
  \s -> s { AST.tower_syncchans = a : AST.tower_syncchans s }

towerPutASTPeriod :: AST.Period -> Tower ()
towerPutASTPeriod a = withAST $ AST.towerInsertPeriod a

towerPutASTSignal :: AST.Signal -> Tower ()
towerPutASTSignal a = withAST $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

towerCodegen :: Codegen a -> Tower a
towerCodegen = Tower . lift

