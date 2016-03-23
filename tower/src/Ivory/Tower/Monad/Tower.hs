{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Tower
  ( Tower(..)
  , Tower'
  , runTower
  , towerNewChannel
  , towerPutMonitor
  , towerPutDependencies
  , towerPutSignalCode
  , towerGetBackend
  ) where

import Prelude ()
import Prelude.Compat

import MonadLib
import Control.Monad.Fix
import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.SignalCode
import Data.Foldable (foldlM)
import Data.List
import Ivory.Tower.Backend
import Ivory.Language

import qualified Ivory.Tower.AST as AST
import qualified Ivory.Language.Syntax.AST as IAST
import Ivory.Language.Proc (Def(DefProc))

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

data TowerOutput = TowerOutput
  { output_monitors :: [AST.Monitor]
  , output_deps :: Dependencies
  , output_sigs :: SignalCode
  }

instance Monoid (TowerOutput) where
  mempty = TowerOutput
    { output_monitors = mempty
    , output_deps = mempty
    , output_sigs = mempty
    }
  mappend a b = TowerOutput
    { output_monitors = output_monitors a `mappend` output_monitors b
    , output_deps = output_deps a `mappend` output_deps b
    , output_sigs = output_sigs a `mappend` output_sigs b
    }

newtype Tower' backend e a = Tower'
  { unTower' :: ReaderT backend (StateT Integer (WriterT (TowerOutput) (Base e))) a
  } deriving (Functor, Monad, Applicative, MonadFix)

backendCallback :: (TowerBackend backend) => backend -> IAST.Proc -> TowerBackendCallback backend
backendCallback backend proc = 
  callbackImpl backend (IAST.procSym proc) (proc)


backendEmitter :: (TowerBackend backend) => backend -> AST.Tower -> AST.Emitter -> TowerBackendEmitter backend
backendEmitter backend ast emit = 
  emitterImpl backend emit $ map (backendHandler backend ast) $
    filter (\x -> AST.handler_chan x == (AST.ChanSync $ AST.emitter_chan emit)) (concat $ map (AST.monitor_handlers) $ AST.tower_monitors ast)

backendHandler :: (TowerBackend backend) => backend -> AST.Tower -> AST.Handler -> TowerBackendHandler backend
backendHandler backend ast han =
    handlerImpl 
      backend 
      han 
      (map (backendEmitter backend ast) $ AST.handler_emitters han) 
      (map (backendCallback backend) $ AST.handler_callbacks han)


backendMonitor :: TowerBackend backend => backend -> AST.Tower -> AST.Monitor -> TowerBackendMonitor backend
backendMonitor backend ast mon =
  let moduledef = put (AST.monitor_module mon) in
  monitorImpl backend mon (map (\x -> SomeHandler $ backendHandler backend ast x) $ AST.monitor_handlers mon) moduledef


runTower :: TowerBackend backend
         => backend
         -> Tower e ()
         -> e
         -> [AST.Tower -> IO AST.Tower]
         -> IO (AST.Tower, TowerBackendOutput backend, Dependencies, SignalCode)
runTower backend t e optslist = do
  a2 <- foldlM (\aaa f -> f aaa) a optslist
  putStrLn $ show $ AST.tower_transformers a2
  let backout = towerImpl backend a2 $ map (backendMonitor backend a2) $ AST.tower_monitors a2
  return (a2, backout, output_deps output, output_sigs output)
  where
  a = mappend (mempty { AST.tower_monitors = mast}) $ 
    mconcat $ flip map (chans) $ \ key ->
    case key of
    AST.ChanSync c -> mempty { AST.tower_syncchans = [c] }
    AST.ChanSignal c -> mempty { AST.tower_signals = [c] }
    AST.ChanPeriod c -> mempty { AST.tower_periods = [c] }
    AST.ChanInit _ -> mempty
  (mast) = output_monitors output
  chans = nub $ map (AST.handler_chan) $ concat $ map (AST.monitor_handlers) mast -- :: [Handler]
  ((), output) = runBase e
    $ runWriterT
    $ fmap fst
    $ runStateT 1
    $ runReaderT backend
    $ unTower'
    $ unTower t

instance BaseUtils (Tower' backend) e where
  freshname n = Tower' $ lift $ lift $ lift $ freshname n
  getEnv = Tower' $ lift $ lift $ lift getEnv

instance BaseUtils Tower e where
  freshname n = Tower $ freshname n
  getEnv = Tower getEnv

towerNewChannel :: Tower e Integer
towerNewChannel = Tower $ Tower' $ sets $ \ n -> (n, n + 1)

towerPutMonitor :: AST.Monitor -> Tower' b e ()
towerPutMonitor ast = Tower' $ put $ mempty { output_monitors = [ast]}

towerPutDependencies :: Dependencies -> Tower e ()
towerPutDependencies d = Tower $ Tower' $ put $ mempty { output_deps = d }

towerPutSignalCode :: SignalCode -> Tower e ()
towerPutSignalCode s = Tower $ Tower' $ put $ mempty { output_sigs = s }

towerGetBackend :: Tower' backend e backend
towerGetBackend = Tower' $ asks id