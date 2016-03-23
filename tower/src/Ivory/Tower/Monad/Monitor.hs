{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor(..)
  , Monitor'
  , monitor
  , externalMonitor
  , monitorPutHandler
  , monitorModuleDef
  , monitorGetBackend
  , liftTower -- XXX UNSAFE TO USE
  ) where

import Prelude ()
import Prelude.Compat

import MonadLib
import Control.Monad.Fix
import Data.Monoid (mconcat)
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST
import Ivory.Language.MemArea (MemArea(..))

import Ivory.Language hiding (Area)
import Ivory.Language.Module (ModuleDef)
import Ivory.Language.Syntax (Module)
import Ivory.Tower.Backend

newtype Monitor e a = Monitor
  { unMonitor :: forall backend. TowerBackend backend => Monitor' backend e a
  }
-- GHC can't derive these trivial instances because of the RankNType.

instance Functor (Monitor e) where
  fmap f (Monitor h) = Monitor $ fmap f h

instance Monad (Monitor e) where
  return x = Monitor $ return x
  Monitor x >>= f = Monitor $ x >>= (unMonitor . f)

instance Applicative (Monitor e) where
  pure = return
  (<*>) = ap

instance MonadFix (Monitor e) where
  mfix f = Monitor $ mfix (unMonitor . f)

newtype Monitor' b e a = Monitor'
  { unMonitor' :: WriterT ([AST.Handler], [Module]) (Tower' b e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

monitor' :: AST.MonitorExternal -> String -> Monitor e () -> Tower e ()
monitor' t n b = Tower $ do
  u <- freshname n
  ((), (hast, modules)) <- runWriterT $ unMonitor' $ unMonitor b
  let mod = mconcat modules
  let ast = AST.Monitor u hast t mod []
  towerPutMonitor ast
    
monitor :: String -> Monitor e () -> Tower e ()
monitor = monitor' AST.MonitorDefined

monitorGetBackend :: Monitor' backend e backend
monitorGetBackend = Monitor' $ lift towerGetBackend

externalMonitor :: String -> Monitor e () -> Tower e ()
externalMonitor = monitor' AST.MonitorExternal

monitorPutHandler :: AST.Handler -> Monitor' b e ()
monitorPutHandler ast = Monitor' $ do
  put ([ast], mempty)

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ Monitor' $ lift $ unTower a

monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef m = 
  Monitor $ Monitor' $ put (mempty,[package "" m])

instance BaseUtils (Monitor' b) e where
  freshname n = Monitor' $ lift $ freshname n
  getEnv = Monitor' $ lift getEnv

instance BaseUtils Monitor e where
  freshname n = Monitor $ freshname n
  getEnv = Monitor getEnv
