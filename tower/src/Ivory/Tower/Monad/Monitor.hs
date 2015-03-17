{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , monitorPutASTHandler
  , monitorModuleDef
  , monitorPutThreadCode
  , liftTower -- XXX UNSAFE TO USE
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid

import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

import Ivory.Language

data MonitorContents = MonitorContents
  { monitorcontents_moddef :: ModuleDef
  , monitorcontents_handlers :: [AST.Handler]
  }

instance Monoid MonitorContents where
  mempty = MonitorContents
    { monitorcontents_moddef = return ()
    , monitorcontents_handlers = mempty
    }
  mappend a b = MonitorContents
    { monitorcontents_moddef = monitorcontents_moddef a >> monitorcontents_moddef b
    , monitorcontents_handlers = monitorcontents_handlers a `mappend` monitorcontents_handlers b
    }

newtype Monitor e a = Monitor
  { unMonitor :: WriterT MonitorContents (Tower e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor e ()
           -> Tower e (AST.Monitor, MonitorCode)
runMonitor n b = do
  u <- freshname n
  ((), mc) <- runWriterT (unMonitor b)
  return (AST.Monitor u $ monitorcontents_handlers mc, MonitorCode $ monitorcontents_moddef mc)

monitorPutASTHandler :: AST.Handler -> Monitor e ()
monitorPutASTHandler h = Monitor $ put $ mempty { monitorcontents_handlers = [h] }

liftTower :: Tower e a -> Monitor e a
liftTower a = Monitor $ lift a

monitorCodegen :: Codegen e a -> Monitor e a
monitorCodegen a = liftTower $ towerCodegen a

monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef m = Monitor $ put $ mempty { monitorcontents_moddef = m }

monitorPutThreadCode :: (AST.Tower -> [ThreadCode]) -> Monitor e ()
monitorPutThreadCode = monitorCodegen . codegenThreadCode

instance BaseUtils Monitor e where
  fresh = liftTower fresh
  getEnv = liftTower getEnv
