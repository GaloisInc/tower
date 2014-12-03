{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.StateMachine.Example where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine

ex :: Monitor e (StateMachine e)
ex  = stateMachine "ex" $ mdo
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
