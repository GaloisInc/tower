{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.StateMachine.Example
  ( ex
  , echo_ex
  ) where

import Data.Char (ord)

import Control.Monad (void)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine

ex :: Monitor e ()
ex  = void $ stateMachine "ex" $ mdo
  initTime <- machineLocal "initTime"
  endInitTime <- machineLocal "endInitTime"
  firstLoopTime <- machineLocal "firstLoopTime"
  latestLoopTime <- machineLocal "latestLoopTime"

  i <- machineStateNamed "init" $ do
        entry $ machineCallback $ \_ -> do
          t <- getTime
          store initTime t
        timeout (500`ms`) $ machineControl $ \_ -> do
          t <- getTime
          store endInitTime t
          return $ goto l

  l <- machineStateNamed "loop" $ do
        entry $ machineCallback $ \_ -> do
          t <- getTime
          store firstLoopTime t
        periodic (50`ms`) $ machineCallback $ \_ -> do
          t <- getTime
          store latestLoopTime t
  return i


echo_ex :: ChanOutput (Stored Uint8)
        -> ChanInput (Stored Uint8)
        -> Monitor e ()
echo_ex istream ostream = do
  sm <- stateMachine "echo" $ mdo
    e <- machineStateNamed "echoing" $ do
            on istream $ do
              out <- machineEmitter ostream 1
              machineControl $ \bref -> do
                b <- deref bref
                when (b /=? (char8 '\n')) $
                  emit out bref
                return $ branch (b ==? (char8 '\n')) d
    d <- machineStateNamed "delaying" $ do
            -- drops istream messages in this state
            timeout (1000`ms`) $ do
              out <- machineEmitter ostream 1
              machineControl $ \_ -> do
                emitV out (char8 '\n')
                return $ goto e
    return e
  stateMachine_onChan sm istream

char8 :: Char -> Uint8
char8 = fromIntegral . ord

