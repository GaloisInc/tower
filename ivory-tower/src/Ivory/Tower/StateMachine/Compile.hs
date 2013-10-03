{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.StateMachine.Compile where

import Control.Monad (forM_)

import Ivory.Tower.StateMachine.Types

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import Ivory.Tower.Node (nodeChannel)

data Runnable =
  Runnable
    { runnable_begin  :: Def ('[]:->())
    , runnable_active :: Def ('[]:->IBool)
    }

begin :: Runnable -> Ivory eff ()
begin r = call_ (runnable_begin r)

active :: Runnable -> Ivory eff IBool
active r = call (runnable_active r)

stateMachine :: String -> MachineM StateLabel -> Task p Runnable
stateMachine name machine = do
  n <- freshname
  m <- withGetTimeMillis
  p <- withPeriodicEvent 1
  newstate <- nodeChannel
  newstate_emitter <- withChannelEmitter (src newstate) "newstateEmitter"
  newstate_receiver <- withChannelEvent (snk newstate) "newstateEvent"
  aux (name ++ n) m p newstate_emitter newstate_receiver
  where
  (istate, states) = runMachineM machine
  aux :: String
      -> OSGetTimeMillis
      -> Event (Stored Uint32)
      -> ChannelEmitter 2 (Stored Uint32)
      -> Event (Stored Uint32)
      -> Task p Runnable
  aux freshn millis tick newstate_emitter newstate_evt = do
    mapM_ mkState states
    taskModuleDef moddef
    return runner
    where
    unique n = n ++ "_machine_" ++ freshn
    runner = Runnable
      { runnable_begin = begin_proc
      , runnable_active = active_proc
      }
    moddef = do
      incl begin_proc
      incl active_proc
      defMemArea active_area
      defMemArea state_area

    active_area = area (unique "state_active")      (Just (ival false))
    state_area  = area (unique "state")             (Just (ival 0))

    (activeState     :: Ref Global (Stored IBool))  = addrOf active_area
    (state           :: Ref Global (Stored Uint32)) = addrOf state_area

    begin_proc = proc (unique "begin") $ body $ do
      a <- deref activeState
      unless a $ noReturn $ do
        store activeState true
        nextstate istate

    active_proc = proc (unique "active") $ body $
      deref activeState >>= ret

    nextstate :: StateLabel -> Ivory (AllocEffects s) ()
    nextstate (StateLabel ns) = do
      -- The first use of nextstate in an event cycle will be the one to be
      -- accepted, since newstate_emitter comes from a channel of size 1.
      emitV_ newstate_emitter (fromIntegral ns)

    handleState :: StateLabel -> Ivory (AllocEffects s) () -> Ivory (ProcEffects s ()) ()
    handleState (StateLabel s) k = do
      current <- deref state
      when (current ==? (fromIntegral s)) (noReturn k)

    mkState :: State -> Task p ()
    mkState (State s maybename handlers) = do
      ehs <- mapM mkHandler handlers
      onEventNamedV newstate_evt nsname $ \newst -> noReturn $
        when (newst ==? (fromIntegral (unStateLabel s))) $ do
          store state newst
          now <- getTimeMillis millis
          currenttime <- local (ival now)
          forM_ ehs $ \(ScopedStatements ss) ->
            mapM_ mkStmt (ss (constRef currenttime))
      where
      nsname = case maybename of
        Just n -> "newstate_" ++ n
        Nothing -> "newstate"
      onEvt :: (IvoryArea area, IvoryZero area)
            => Event area
            -> (forall s s' . ConstRef s area -> Ivory (ProcEffects s' ()) ())
            -> Task p ()
      onEvt e k = case maybename of
        Just n -> onEventNamed e n k
        Nothing -> onEvent e k
      mkHandler (EntryHandler stmts) = return stmts -- XXX correct?
      mkHandler (TimeoutHandler i stmts) = do
        due <- taskLocal (unique ("timeoutDue" ++ show i))
        onEvt tick $ \currenttime -> handleState s $ do
          now <- deref currenttime
          d <- deref due
          when ((d >? 0) .&& (d <=? now)) $ case stmts of
            ScopedStatements ss -> do
              mapM_ mkStmt (ss currenttime)
              store due 0
        return $ scopedIvory $ \starttimeRef -> do
          t <- deref starttimeRef
          store due (t + (fromIntegral i))

      mkHandler (PeriodHandler i stmts) = do
        due <- taskLocal (unique ("periodDue" ++ show i))
        onEvt tick $ \currenttime -> case stmts of
          ScopedStatements ss -> handleState s $ do
            now <- deref currenttime
            d   <- deref due
            when (d <=? now) $ do
              mapM_ mkStmt (ss currenttime)
              store due (d + (fromIntegral i))
        return $ scopedIvory $ \starttimeRef -> do
          t <- deref starttimeRef
          store due (t + (fromIntegral i))

      mkHandler (EventHandler evt stmts) = do
        onEvt evt $ case stmts of
          ScopedStatements ss -> \v -> handleState s $ do
            mapM_ mkStmt (ss v)
        return $ scopedIvory $ const (return ())

    mkStmt :: Stmt s -> Ivory (AllocEffects s) ()
    mkStmt (Stmt s) = do
      cflowM <- s
      foldr mkCFlow (return ()) (runCFlowM cflowM)

    mkCFlow :: CFlowAST -> Ivory (AllocEffects s) () -> Ivory (AllocEffects s) ()
    mkCFlow (CFlowBranch b lbl) acc = ifte_ b (nextstate lbl) acc
    mkCFlow (CFlowHalt   b)     acc = ifte_ b (store activeState false) acc


