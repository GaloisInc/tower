{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.Tower
  ( Tower()
  , GeneratedCode()
  , runTower
  , ChanInput()
  , ChanOutput()
  , channel
  , signal
  , signalUnsafe
  , Signalable(..)
  , module Ivory.Tower.Types.Time
  , period
  , periodPhase
  , systemInit
  , Monitor()
  , monitor
  , externalMonitor
  , towerModule
  , towerDepends
  , towerArtifact

  , getTime
  , BaseUtils(..)
  , Unique
  , showUnique
  ) where

import qualified Data.Map as Map
import Data.Monoid
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.SignalCode
import Ivory.Tower.Types.Signalable
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.Unique

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor

import Ivory.Language
import Ivory.Artifact
import Ivory.Artifact.Location
import qualified Ivory.Language.Area as I

channel :: IvoryArea a => Tower e (ChanInput a, ChanOutput a)
channel = do
  -- Channels are anonymous so `freshname` is not an appropriate way to
  -- give them unique names. Instead, keep a count of the number of
  -- channels created in this Tower.
  f <- towerNewChannel
  let ast = AST.SyncChan f (I.ivoryArea (chanProxy c))
      c = Chan (AST.ChanSync ast)
  return (ChanInput c, ChanOutput c)
  where
  chanProxy :: Chan a -> Proxy a
  chanProxy _ = Proxy

-- Note: signals are no longer tied to be the same type throughout
-- a given Tower. We'd need to add another phantom type to make that
-- work.
signal :: (Time a, Signalable s)
       => s -> a -> Tower e (ChanOutput (Stored ITime))
signal s t = signalUnsafe s t (return ())

signalUnsafe :: (Time a, Signalable s)
       => s -> a -> (forall eff . Ivory eff ())
       -> Tower e (ChanOutput (Stored ITime))
signalUnsafe s t i = do
  towerPutSignalCode $ SignalCode
    { signalcode_init = signalInit s
    , signalcode_signals = Map.singleton n $
        GeneratedSignal $ \i' -> signalHandler s (i >> i')
    }
  return (ChanOutput (Chan (AST.ChanSignal ast)))
  where
  n = signalName s
  ast = AST.Signal
    { AST.signal_name = n
    , AST.signal_deadline = microseconds t
    }

period :: Time a => a -> Tower e (ChanOutput (Stored ITime))
period t = periodPhase t (0`us`)

periodPhase :: (Time a, Time b)
       => a
       -> b
       -> Tower e (ChanOutput (Stored ITime))
periodPhase t ph = do
  let ast = AST.Period (microseconds t) perTy (microseconds ph)
  return (ChanOutput (Chan (AST.ChanPeriod ast)))
  where perTy = I.ivoryArea (Proxy :: I.AProxy (Stored ITime))

systemInit :: ChanOutput (Stored ITime)
systemInit = ChanOutput (Chan (AST.ChanInit AST.Init))

towerModule :: Module -> Tower e ()
towerModule m = towerPutDependencies $ mempty { dependencies_modules = [m] }

towerDepends :: Module -> Tower e ()
towerDepends m = towerPutDependencies $ mempty { dependencies_depends = [m] }

towerArtifact :: Located Artifact -> Tower e ()
towerArtifact a = towerPutDependencies $ mempty { dependencies_artifacts = [a] }

getTime :: Ivory eff ITime
getTime = call getTimeProc
  where
  -- Must be provided by the code generator:
  getTimeProc :: Def('[]:->ITime)
  getTimeProc = importProc "tower_get_time" "tower_time.h"

