{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Coroutine where

import Control.Applicative
import Ivory.Language
import Ivory.Tower.Handler
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monitor
import Ivory.Tower.Tower
import Ivory.Tower.Backend

-- | Lifts an Ivory coroutine into a set of Tower handlers. Takes the sink of
-- two channels: (1) whether the coroutine should be initialized @chanInit@, and
-- a (2) value to process over the @chan@ channel. The value of @chanInit@ is
-- ignored; whenever a message is received over the channel, the coroutine is
-- reinitialized. Otherwise, values from the @chan@ channel are passed to the
-- coroutine. Example usage:
--
-- @
--     coroutineHandler initChan resChan "foo" $ do
--       e <- emitter chan 1
--       return $ CoroutineBody $ \yield -> do
--         ... Ivory code ...
-- @
coroutineHandler :: (IvoryArea init, IvoryZero init, IvoryArea a, IvoryZero a)
                 => ChanOutput init
                 -> ChanOutput a
                 -> String
                 -> (forall evt. Handler evt e (CoroutineBody a))
                 -> Monitor e ()
coroutineHandler chanInit chan name block = do
  (doInitChan, readyChan) <- liftTower channel
  lastValue <- state "last_value"

  handler readyChan name $ Handler $ do
    be <- handlerGetBackend
    u <- handlerUnique
    let (be', nm) = uniqueImpl be u
    handlerSetBackend be'
    coro <- coroutine nm <$> (unHandler block)
    liftMonitor $ Monitor $ monitorModuleDef $ coroutineDef coro
    unHandler $ callbackV $ \ shouldInit -> coroutineRun coro shouldInit $ constRef lastValue

  handler chanInit (name ++ "_init") $ do
    doInit <- emitter doInitChan 1
    callback $ const $ emitV doInit true

  handler chan (name ++ "_raw") $ do
    doInit <- emitter doInitChan 1
    callback $ \ ref -> do
      refCopy lastValue ref
      emitV doInit false
