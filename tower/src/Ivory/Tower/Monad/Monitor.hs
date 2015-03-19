{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor(..)
  , Monitor'
  , monitor
  , monitorGetBackend
  , monitorPutHandler
  , monitorModuleDef
  , liftTower -- XXX UNSAFE TO USE
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid
import Ivory.Tower.Backend
import Ivory.Tower.Backend.Compat
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

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
  { unMonitor' :: ReaderT backend (WriterT ([AST.Handler], [SomeHandler backend], ModuleDef) (Tower e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

monitor :: String -> Monitor e () -> Tower e ()
monitor n b = do
  u <- freshname n
  ((), (hast, handlers, moddef)) <- runWriterT $ runReaderT CompatBackend $ unMonitor' $ unMonitor b
  let ast = AST.Monitor u hast
  towerPutASTMonitor ast
  let (CompatMonitor gc) = monitorImpl CompatBackend ast handlers moddef
  towerCodegen $ codegenMonitor gc

monitorGetBackend :: Monitor' backend e backend
monitorGetBackend = Monitor' ask

monitorPutHandler :: AST.Handler -> TowerBackendHandler backend a -> Monitor' backend e ()
monitorPutHandler ast h = Monitor' $ put ([ast], [SomeHandler h], mempty)

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ Monitor' $ lift $ lift a

monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef m = Monitor $ Monitor' $ put (mempty, mempty, m)

instance BaseUtils (Monitor' backend) e where
  fresh = Monitor' $ lift $ lift fresh
  getEnv = Monitor' $ lift $ lift getEnv

instance BaseUtils Monitor e where
  fresh = Monitor fresh
  getEnv = Monitor getEnv
