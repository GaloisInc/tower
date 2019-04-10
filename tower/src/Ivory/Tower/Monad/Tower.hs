{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Tower
  ( Tower(..)
  , Tower'
  , runTower
  , runTower_
  , towerGetBackend
  , towerGetHandlers
  , towerNewChannel
  , towerPutHandler
  , towerPutMonitor
  , towerPutDependencies
  , towerPutSignalCode
  ) where

import Prelude ()
import Prelude.Compat

import MonadLib
import Control.Monad.Fix
import Data.Semigroup
import Ivory.Tower.Backend
import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Chan
import qualified Ivory.Tower.Types.ChanMap as ChanMap
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.SignalCode

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
  deriving (Semigroup, Monoid)

type Sinks backend = ChanMap.ChanMap (SinkList backend)

data TowerOutput backend = TowerOutput
  { output_sinks :: Sinks backend
  , output_monitors :: [(AST.Monitor, TowerBackendMonitor backend)]
  , output_deps :: Dependencies
  , output_sigs :: SignalCode
  }

instance Semigroup (TowerOutput backend) where
  (<>) a b = TowerOutput
    { output_sinks = ChanMap.unionWith mappend (output_sinks a) (output_sinks b)
    , output_monitors = output_monitors a `mappend` output_monitors b
    , output_deps = output_deps a `mappend` output_deps b
    , output_sigs = output_sigs a `mappend` output_sigs b
    }

instance Monoid (TowerOutput backend) where
  mempty = TowerOutput
    { output_sinks = ChanMap.empty
    , output_monitors = mempty
    , output_deps = mempty
    , output_sigs = mempty
    }

newtype Tower' backend e a = Tower'
  { unTower' :: ReaderT (backend, Sinks backend) (StateT Integer (WriterT (TowerOutput backend) (Base e))) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTower :: TowerBackend backend
         => backend
         -> Tower e a
         -> e
         -> (a, AST.Tower, TowerBackendOutput backend, Dependencies, SignalCode)
runTower backend t e = (a, ast, towerImpl backend ast monitors, output_deps output, output_sigs output)
  where
  ast = mappend (mempty { AST.tower_monitors = mast }) $ mconcat $ flip map (ChanMap.keys sinks) $ \ key ->
    case key of
    AST.ChanSync c -> mempty { AST.tower_syncchans = [c] }
    AST.ChanSignal c -> mempty { AST.tower_signals = [c] }
    AST.ChanPeriod c -> mempty { AST.tower_periods = [c] }
    AST.ChanInit _ -> mempty
  (mast, monitors) = unzip $ output_monitors output
  sinks = output_sinks output
  (a, output) = runBase e
    $ runWriterT
    $ fmap fst
    $ runStateT 1
    $ runReaderT (backend, sinks)
    $ unTower'
    $ unTower t

runTower_ :: TowerBackend backend
          => backend
          -> Tower e ()
          -> e
          -> (AST.Tower, TowerBackendOutput backend, Dependencies, SignalCode)
runTower_ b t e =
  let ((), ast, out, deps, sig) = runTower b t e
  in (ast, out, deps, sig)

instance BaseUtils (Tower' backend) e where
  freshname n = Tower' $ lift $ lift $ lift $ freshname n
  getEnv = Tower' $ lift $ lift $ lift getEnv

instance BaseUtils Tower e where
  freshname n = Tower $ freshname n
  getEnv = Tower getEnv

towerGetBackend :: Tower' backend e backend
towerGetBackend = Tower' $ asks fst

towerGetHandlers :: Chan b -> Tower' backend e [TowerBackendHandler backend b]
towerGetHandlers chan = Tower' $ do
  sinks <- asks snd
  return $ maybe [] unSinkList $ ChanMap.lookup chan sinks

towerNewChannel :: Tower e Integer
towerNewChannel = Tower $ Tower' $ sets $ \ n -> (n, n + 1)

towerPutHandler :: Chan a -> TowerBackendHandler backend a -> Tower' backend e ()
towerPutHandler chan h = Tower' $ put $
  mempty { output_sinks = ChanMap.singleton chan $ SinkList [h] }

towerPutMonitor :: AST.Monitor -> TowerBackendMonitor backend -> Tower' backend e ()
towerPutMonitor ast m = Tower' $ put $ mempty { output_monitors = [(ast, m)] }

towerPutDependencies :: Dependencies -> Tower e ()
towerPutDependencies d = Tower $ Tower' $ put $ mempty { output_deps = d }

towerPutSignalCode :: SignalCode -> Tower e ()
towerPutSignalCode s = Tower $ Tower' $ put $ mempty { output_sigs = s }
