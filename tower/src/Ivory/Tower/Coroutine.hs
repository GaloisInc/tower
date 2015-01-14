{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Coroutine where

import Control.Monad.Fix
import Ivory.Language
import Ivory.Tower.Handler
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monitor
import Ivory.Tower.Tower
import Ivory.Tower.Types.Chan

coroutineHandler :: (IvoryArea a, IvoryZero a) => ChanOutput a -> (forall evt. Handler evt e (Coroutine a)) -> Monitor e ()
coroutineHandler chan block = do
  (doInitChan, (ChanOutput (Chan readyast))) <- liftTower channel
  lastValue <- state "last_value"

  name <- mfix $ \ name -> runHandler name readyast $ do
    coro <- block
    liftMonitor $ monitorModuleDef $ coroutineDef coro
    callbackV $ \ shouldInit -> coroutineRun coro shouldInit $ constRef lastValue
    return $ coroutineName coro

  handler systemInit (name ++ "_init") $ do
    doInit <- emitter doInitChan 1
    callback $ const $ emitV doInit true

  handler chan (name ++ "_raw") $ do
    doInit <- emitter doInitChan 1
    callback $ \ ref -> do
      refCopy lastValue ref
      emitV doInit false
