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
  , towerGetHandlers
  , towerPutHandler
  , towerPutMonitor
  , towerCodegen
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid
import Ivory.Tower.Backend
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Types.Chan
import qualified Ivory.Tower.Types.ChanMap as ChanMap
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

newtype SinkList backend a = SinkList { unSinkList :: [TowerBackendHandler backend a] }
  deriving Monoid

instance Monoid (ChanMap.ChanMap (SinkList backend)) where
  mempty = ChanMap.empty
  mappend = ChanMap.unionWith mappend

type Sinks backend = ChanMap.ChanMap (SinkList backend)

newtype Tower' backend e a = Tower'
  { unTower' :: ReaderT (backend, Sinks backend) (WriterT (Sinks backend, [AST.Monitor], [TowerBackendMonitor backend]) (Codegen e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: TowerBackend backend
         => backend
         -> Tower e ()
         -> e
         -> (AST.Tower, TowerBackendOutput backend, GeneratedCode)
runTower backend t e = (a, towerImpl backend a monitors, b)
  where
  a = mappend (mempty { AST.tower_monitors = mast }) $ mconcat $ flip map (ChanMap.keys sinks) $ \ key ->
    case key of
    AST.ChanSync c -> mempty { AST.tower_syncchans = [c] }
    AST.ChanSignal c -> mempty { AST.tower_signals = [c] }
    AST.ChanPeriod c -> mempty { AST.tower_periods = [c] }
    AST.ChanInit _ -> mempty
  (((), (sinks, mast, monitors)), b) = runBase e
    $ runCodegen
    $ runWriterT
    $ runReaderT (backend, sinks)
    $ unTower'
    $ unTower t

instance BaseUtils (Tower' backend) e where
  fresh = Tower' $ lift $ lift fresh
  getEnv = Tower' $ lift $ lift getEnv

instance BaseUtils Tower e where
  fresh = Tower fresh
  getEnv = Tower getEnv

towerGetBackend :: Tower' backend e backend
towerGetBackend = Tower' $ asks fst

towerGetHandlers :: Chan b -> Tower' backend e [TowerBackendHandler backend b]
towerGetHandlers chan = Tower' $ do
  sinks <- asks snd
  return $ maybe [] unSinkList $ ChanMap.lookup chan sinks

towerPutHandler :: Chan a -> TowerBackendHandler backend a -> Tower' backend e ()
towerPutHandler chan h = Tower' $ put (ChanMap.singleton chan $ SinkList [h], mempty, mempty)

towerPutMonitor :: AST.Monitor -> TowerBackendMonitor backend -> Tower' backend e ()
towerPutMonitor ast m = Tower' $ put (mempty, [ast], [m])

towerCodegen :: Codegen e a -> Tower e a
towerCodegen x = Tower $ Tower' $ lift $ lift x
