{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.StateMachine
  ( on
  , periodic
  , timeout
  , entry

  , MachineM
  , StateLabel

  , machineState
  , machineStateNamed
  , branch
  , goto
  , haltWhen
  , halt

  , machineLocal
  , machineLocalInit

  , machineControl
  , machineCallback
  , machineEmitter

  , module Ivory.Tower.StateMachine.Compile
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine.Monad
import Ivory.Tower.StateMachine.Compile

on :: forall area e
    . (IvoryArea area, IvoryZero area)
   => ChanOutput area
   -> StmtM area e ()
   -> StateM e ()
on c s = writeStateHandler
       $ ChanStateHandler c
       $ s

entry :: StmtM (Stored MachineState) e () -> StateM e ()
entry s = writeStateHandler
        $ EntryStateHandler s

timeout :: Time a => a -> StmtM (Stored ITime) e () -> StateM e ()
timeout t s = writeStateHandler
            $ TimeoutStateHandler (microseconds t) s

periodic :: Time a => a -> StmtM (Stored ITime) e () -> StateM e ()
periodic t s = writeStateHandler
             $ PeriodStateHandler (microseconds t) s

machineState :: StateM e () -> MachineM e StateLabel
machineState sh = stateAux sh Nothing

machineStateNamed :: String -> StateM e () -> MachineM e StateLabel
machineStateNamed n sh = stateAux sh (Just n)

stateAux :: StateM e () -> Maybe String-> MachineM e StateLabel
stateAux sh name = do
  l <- label
  writeState $ runStateM sh l name
  return l

branch :: (CFlowable m) => IBool -> StateLabel -> m ()
branch b lbl = cflow $ CFlowBranch b lbl

goto :: (CFlowable m) => StateLabel -> m ()
goto lbl = branch true lbl

haltWhen :: (CFlowable m) => IBool -> m ()
haltWhen p = cflow $ CFlowHalt p

halt :: (CFlowable m) => m ()
halt = haltWhen true

machineCallback :: (IvoryArea a, IvoryZero a)
                => (forall s s'. ConstRef s a -> Ivory (AllocEffects s') ())
                -> StmtM a e ()
machineCallback k = machineControl (\a -> k a >> return (return ()))

