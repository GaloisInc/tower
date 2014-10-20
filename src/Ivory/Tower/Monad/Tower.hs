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
  , towerPutModules
  , towerPutThreadCode
  , towerGetGeneratedAST
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Generated

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode

import Ivory.Tower.ToyObjLang

import qualified Ivory.Tower.AST as AST

newtype Tower a = Tower
  { unTower :: StateT AST.Tower Generated a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: Tower () -> (AST.Tower, GeneratedCode)
runTower t = (a,b)
  where
  (a,b) = runBase (runGenerated outer a)
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
towerPutASTPeriod a = withAST $
  \s -> s { AST.tower_periods = a : AST.tower_periods s }

towerPutASTSignal :: AST.Signal -> Tower ()
towerPutASTSignal a = withAST $
  \s -> s { AST.tower_signals = a : AST.tower_signals s }

towerPutModules :: (AST.Tower -> [Module]) -> Tower ()
towerPutModules m = Tower $ lift $ codegenPutModules m

towerPutThreadCode :: (AST.Tower -> [ThreadCode])
                     -> Tower ()
towerPutThreadCode c = Tower $ lift $ codegenPutThreadCode c

towerGetGeneratedAST :: Tower AST.Tower
towerGetGeneratedAST = Tower $ lift $ codegenGetGeneratedAST


