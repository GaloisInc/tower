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

newtype Tower p a = Tower
  { unTower :: StateT AST.Tower Codegen a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower p () -> (AST.Tower, GeneratedCode)
runTower t = (a,b)
  where
  (a,b) = runBase (runCodegen outer a)
  outer = fmap snd (runStateT AST.emptyTower (unTower t))

instance BaseUtils (Tower p) where
  fresh = Tower $ lift fresh

withAST :: (AST.Tower -> AST.Tower) -> Tower p ()
withAST f = Tower $ do
  a <- get
  set (f a)

towerPutASTMonitor :: AST.Monitor -> Tower p ()
towerPutASTMonitor m = withAST $
  \s -> s { AST.tower_monitors = m : AST.tower_monitors s }

towerPutASTSyncChan :: AST.SyncChan -> Tower p ()
towerPutASTSyncChan a = withAST $
  \s -> s { AST.tower_syncchans = a : AST.tower_syncchans s }

towerPutASTPeriod :: AST.Period -> Tower p ()
towerPutASTPeriod a = withAST $ AST.towerInsertPeriod a

towerPutASTSignal :: AST.Signal -> Tower p ()
towerPutASTSignal a = withAST $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

towerCodegen :: Codegen a -> Tower p a
towerCodegen = Tower . lift

