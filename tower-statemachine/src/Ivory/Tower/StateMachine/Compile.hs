{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.StateMachine.Compile
  ( StateMachine
  , stateMachine_onChan
  , stateMachine
  ) where

import Unsafe.Coerce
import Control.Monad (forM_, mapAndUnzipM)

import Ivory.Tower.StateMachine.Monad

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Monad.Monitor (liftTower)
import Ivory.Stdlib

data StateMachine e =
  StateMachine
    { stateMachine_onChan :: forall a . (IvoryArea a, IvoryZero a)
                          => ChanOutput a -> Monitor e ()
    }

stateMachine :: forall e . String -> MachineM e StateLabel
             -> Monitor e (StateMachine e)
stateMachine name machine = do
  uniq <- freshname ("machine_" ++ name)
  tick <- liftTower (period (Milliseconds 1))
  newstate <- liftTower channel
  aux uniq tick (fst newstate) (snd newstate)
  where
  aux :: Unique
      -> ChanOutput (Stored ITime)
      -> ChanInput  (Stored MachineState)
      -> ChanOutput (Stored MachineState)
      -> Monitor e (StateMachine e)
  aux uniq tick newstate_in newstate_out = do
    (istate, states) <- runMachineM machine
    mstate       <- state "machineState"
    handler systemInit (named "init") $ do
      newstate_e <- emitter newstate_in 1
      callback $ \_ -> emitV newstate_e (stateLabel istate)
    makeHandlers mstate states
    return StateMachine
      { stateMachine_onChan = \c ->
          makeChanHandler c mstate states
      }
    where
    named n = (showUnique uniq) ++ "_machine_" ++ n

    zipHandlers a b = do
      (cs, ds) <- mapAndUnzipM a b
      return (sequence_ cs, \e s -> forM_ ds (\d -> d e s))

    makeHandlers :: Ref Global (Stored MachineState)
                 -> [State e] -> Monitor e ()
    makeHandlers mstate ss = do
      (ns_timeout_hs, tick_timeout_hs) <- zipHandlers timeoutHandler
          (timeoutStateHandlers ss)
      (ns_period_hs, tick_period_hs) <- zipHandlers periodHandler
          (periodStateHandlers ss)

      handler newstate_out (named "newstate") $ do
        newstate_e <- emitter newstate_in 1
        callbackV $ \s -> do
          store mstate s
        ns_timeout_hs
        ns_period_hs
        forM_ (entryStateHandlers ss) $ \(lbl, stmtm) -> do
          runStmtM stmtm newstate_e (undefined lbl mstate)

      handler tick (named "tick") $ do
        newstate_e <- emitter newstate_in 1
        tick_timeout_hs newstate_e mstate
        tick_period_hs newstate_e mstate


    makeChanHandler :: (IvoryArea a, IvoryZero a)
                     => ChanOutput a
                     -> Ref Global (Stored MachineState)
                     -> [State e] -> Monitor e ()
    makeChanHandler cout mstate ss = do
      handler cout (named "chan") $ do
        newstate_e <- emitter newstate_in 1
        forM_ (chanStateHandlers ss cout) $ \(lbl, stmtm) -> do
          runStmtM stmtm newstate_e (undefined lbl mstate)

entryStateHandlers :: [State e]
  -> [(StateLabel, StmtM (Stored MachineState) e ())]
entryStateHandlers ss =
  [ (lbl, m)
  | (State lbl _ hs) <- ss
  , (EntryStateHandler m) <- hs
  ]

timeoutStateHandlers :: [State e]
  -> [(StateLabel, Microseconds, StmtM (Stored ITime) e ())]
timeoutStateHandlers ss =
  [ (lbl, t, m)
  | (State lbl _ hs) <- ss
  , (TimeoutStateHandler t m) <- hs
  ]

periodStateHandlers :: [State e]
  -> [(StateLabel, Microseconds, StmtM (Stored ITime) e ())]
periodStateHandlers ss =
  [ (lbl, t, m)
  | (State lbl _ hs) <- ss
  , (PeriodStateHandler t m) <- hs
  ]

chanStateHandlers :: forall a e
                   . [State e]
                  -> ChanOutput a
                  -> [(StateLabel, StmtM a e ())]
chanStateHandlers ss c =
  [ (lbl, unsafeCoerce m)
  | (State lbl _ hs) <- ss
  , (ChanStateHandler co m) <- hs
  , unsafeCoerce co == c
  ]


timeoutHandler  :: ( StateLabel
                   , Microseconds
                   , StmtM (Stored ITime) e ())
                 -> Monitor e
                      ( Handler (Stored MachineState) e ()
                      ,      Emitter (Stored MachineState)
                          -> Ref Global (Stored MachineState)
                          -> Handler (Stored ITime) e ()
                      )

timeoutHandler (lbl, t, stmtm) = do
  has_run <- state (named "has_run")
  deadline <- state (named "deadline")
  let reset = callback $ \_ -> do
        now <- getTime
        store deadline (now + toITime t)
        store has_run false
      trigger e mstate = runStmtM stmtm e $ ICflow $ \k -> do
        ms <- deref mstate
        r <- deref has_run
        dl <- deref deadline
        now <- getTime
        when ( ms ==? stateLabel lbl
              .&& iNot r
              .&& now >=? dl) $
          store has_run true >> k

  return (reset, trigger)
  where
  named = id -- XXX make more descriptive

periodHandler  :: ( StateLabel
                   , Microseconds
                   , StmtM (Stored ITime) e ())
                 -> Monitor e
                      ( Handler (Stored MachineState) e ()
                      ,      Emitter (Stored MachineState)
                          -> Ref Global (Stored MachineState)
                          -> Handler (Stored ITime) e ()
                      )

periodHandler (lbl, t, stmtm) = do
  deadline <- state (named "deadline")
  let reset = callback $ \_ -> do
        now <- getTime
        store deadline (now + toITime t)
      trigger e mstate = runStmtM stmtm e $ ICflow $ \k -> do
        ms <- deref mstate
        dl <- deref deadline
        now <- getTime
        when ( ms ==? stateLabel lbl
              .&& now >=? dl) $
          store deadline (now + toITime t) >> k

  return (reset, trigger)
  where
  named = id -- XXX make more descriptive

