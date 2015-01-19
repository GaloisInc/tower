{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Coroutine where

import Control.Applicative
import Ivory.Language
import Ivory.Tower.Handler
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monitor
import Ivory.Tower.Tower

coroutineHandler :: (IvoryArea init, IvoryZero init, IvoryArea a, IvoryZero a) => ChanOutput init -> ChanOutput a -> String -> (forall evt. Handler evt e (CoroutineBody a)) -> Monitor e ()
coroutineHandler chanInit chan name block = do
  (doInitChan, readyChan) <- liftTower channel
  lastValue <- state "last_value"

  handler readyChan name $ do
    coro <- coroutine <$> fmap showUnique handlerName <*> block
    liftMonitor $ monitorModuleDef $ coroutineDef coro
    callbackV $ \ shouldInit -> coroutineRun coro shouldInit $ constRef lastValue

  handler chanInit (name ++ "_init") $ do
    doInit <- emitter doInitChan 1
    callback $ const $ emitV doInit true

  handler chan (name ++ "_raw") $ do
    doInit <- emitter doInitChan 1
    callback $ \ ref -> do
      refCopy lastValue ref
      emitV doInit false
