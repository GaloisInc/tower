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
  , monitorGetBackend
  , monitorSetBackend
  , monitorGetHandlers
  , monitorPutHandler
  , monitorModuleDef
  , monitorModuleDef'
  , liftTower -- XXX UNSAFE TO USE
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid
import Ivory.Tower.Backend
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Chan

import Ivory.Language

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

newtype Monitor' backend e a = Monitor'
  { unMonitor' :: WriterT ([AST.Handler], [SomeHandler backend], ModuleDef) (Tower' backend e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

instance StateM (Monitor' backend e) backend where
  get = Monitor' get
  set = Monitor' . set

monitor' :: AST.MonitorExternal -> String -> Monitor e () -> Tower e ()
monitor' t n b = Tower $ do
  u <- freshname n
  ((), (hast, handlers, moddef)) <- runWriterT $ unMonitor' $ unMonitor b
  let ast = AST.Monitor u hast t
  m <- monitorImpl ast handlers moddef
  towerPutMonitor ast m

monitor :: String -> Monitor e () -> Tower e ()
monitor = monitor' AST.MonitorDefined

externalMonitor :: String -> Monitor e () -> Tower e ()
externalMonitor = monitor' AST.MonitorExternal

monitorGetBackend :: Monitor' backend e backend
monitorGetBackend = Monitor' $ lift towerGetBackend

monitorSetBackend :: backend -> Monitor' backend e ()
monitorSetBackend be = Monitor' $ lift $ towerSetBackend be

monitorGetHandlers :: Chan b -> Monitor' backend e [TowerBackendHandler backend b]
monitorGetHandlers chan = Monitor' $ lift $ towerGetHandlers chan

monitorPutHandler :: AST.Handler
                  -> Chan a
                  -> (backend, TowerBackendHandler backend a)
                  -> Monitor' backend e ()
monitorPutHandler ast chan (be, h) = Monitor' $ do
  put ([ast], [SomeHandler h], mempty)
  lift $ towerSetBackend be
  lift $ towerPutHandler chan h

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ Monitor' $ lift $ unTower a

monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef m = Monitor (monitorModuleDef' m)

monitorModuleDef' :: ModuleDef -> Monitor' backend e ()
monitorModuleDef' m = Monitor' $ put (mempty, mempty, m)

instance BaseUtils (Monitor' backend) e where
  fresh = Monitor' $ lift fresh
  getEnv = Monitor' $ lift getEnv

instance BaseUtils Monitor e where
  fresh = Monitor fresh
  getEnv = Monitor getEnv
