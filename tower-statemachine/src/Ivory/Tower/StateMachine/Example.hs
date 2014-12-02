{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.StateMachine.Example where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine

ex :: Monitor e (StateMachine e)
ex  = stateMachine "ex" $ mdo
  initTime <- machineLocal "initTime"
  firstLoopTime <- machineLocal "firstLoopTime"
  latestLoopTime <- machineLocal "latestLoopTime"

  i <- machineStateNamed "init" $ do
        entry $ machineCallback $ \_ -> do
          t <- getTime
          store initTime t
        timeout (100`ms`) $ machineControl $ \_ ->
          return $ goto l
  l <- machineStateNamed "loop" $ do
        entry $ machineCallback $ \_ -> do
          t <- getTime
          store firstLoopTime t
        periodic (333`ms`) $ machineCallback $ \_ -> do
          t <- getTime
          store latestLoopTime t
  return i
