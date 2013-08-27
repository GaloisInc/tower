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
  aux (name ++ n) m p
  where
  (istate, states) = runMachineM machine
  aux :: String -> OSGetTimeMillis -> Event (Stored Uint32) -> Task p Runnable

  aux freshn millis tick = do
    mapM_ mkState states
    taskModuleDef moddef
    return runner
    where
    unique n = n ++ "_machine_" ++ freshn
    runner = Runnable
      { runnable_begin = begin
      , runnable_active = active
      }
    moddef = do
      incl begin
      incl active
      defMemArea active_area
      defMemArea statet_area
      defMemArea state_area

    active_area = area (unique "state_active")      (Just (ival false))
    statet_area = area (unique "state_change_time") (Just (ival 0))
    state_area  = area (unique "state")             (Just (ival 0))

    (activeState     :: Ref Global (Stored IBool))  = addrOf active_area
    (stateChangeTime :: Ref Global (Stored Uint32)) = addrOf statet_area
    (state           :: Ref Global (Stored Uint32)) = addrOf state_area

    begin = proc (unique "begin") $ body $ do
      a <- deref activeState
      unless a $ noReturn $ do
        store activeState true
        nextstate istate
    active = proc (unique "active") $ body $
      deref activeState >>= ret

    nextstate :: StateLabel -> Ivory (AllocEffects s) ()
    nextstate (StateLabel ns) = do
      t <- getTimeMillis millis
      store stateChangeTime t
      store state (fromIntegral ns)

    handleState :: StateLabel -> Ivory (AllocEffects s) () -> Ivory (ProcEffects s ()) ()
    handleState (StateLabel s) k = do
      current <- deref state
      when (current ==? (fromIntegral s)) (noReturn k)

    mkState :: State -> Task p ()
    mkState (State s handlers) = forM_ handlers $ \h -> case h of
      TimeoutHandler i stmts -> onEvent tick $ \currenttime ->
        case stmts of
          ScopedStatements ss -> handleState s $ do
            now     <- deref currenttime
            changet <- deref stateChangeTime
            when ((now - changet) >=? (fromIntegral i)) $ do
              mapM_ mkStmt (ss currenttime)
      EventHandler evt stmts ->
        onEvent evt $ case stmts of
          ScopedStatements ss -> \v -> handleState s $ do
            mapM_ mkStmt (ss v)

    mkStmt :: Stmt s -> Ivory (AllocEffects s) ()
    mkStmt (Stmt s) = do
      cflowM <- s
      foldl mkCFlow (return ()) (runCFlowM cflowM)

    mkCFlow :: Ivory (AllocEffects s) () -> CFlowAST -> Ivory (AllocEffects s) ()
    mkCFlow acc (CFlowBranch b lbl) = ifte_ b (nextstate lbl) acc
    mkCFlow acc (CFlowHalt   b)     = ifte_ b (store activeState false) acc


