{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.StateMachine.Compile where

import Control.Monad (forM_)
import Data.List (find)

import Ivory.Tower.StateMachine.Monad

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Monad.Monitor (liftTower)
import Ivory.Stdlib

data Runnable =
  Runnable
    { runnable_begin  :: forall a e . Handler a e ScopedIvory
    , runnable_active :: Def ('[]:->IBool)
    }

newtype ScopedIvory = ScopedIvory (forall s . Ivory (AllocEffects s) ())

begin :: Runnable -> Handler a e ScopedIvory
begin r = runnable_begin r

active :: Runnable -> Ivory eff IBool
active r = call (runnable_active r)

stateMachine :: forall e . String -> MachineM e StateLabel -> Monitor e Runnable
stateMachine name machine = do
  uniq <- freshname ("machine_" ++ name)
  tick <- liftTower (period (Milliseconds 1))
  newstate <- liftTower channel
  aux uniq tick (fst newstate) (snd newstate)
  where
  aux :: Unique
      -> ChanOutput (Stored ITime)
      -> ChanInput  (Stored Uint32)
      -> ChanOutput (Stored Uint32)
      -> Monitor e Runnable
  aux uniq tick newstate_in newstate_out = do
    (istate, states) <- runMachineM machine
    let begin_h = do
          e <- emitter newstate_in 1
          return $ ScopedIvory $ do
            a <- deref machine_active
            unless a $ noReturn $ do
              store machine_active true
              nextstate e istate

        statename :: StateLabel -> String
        statename lbl = case find aux' states of
          Just (State _ (Just n) _) -> "state " ++ n
          _ -> "unnamed state #" ++ show (unStateLabel lbl)
          where
          aux' (State l _ _) = l == lbl
        runner = Runnable
          { runnable_begin = begin_h 
          , runnable_active = active_proc
          }
        moddef = do
          incl active_proc
          defMemArea machine_active_area
          defMemArea machine_state_area

        nextstate :: Emitter (Stored Uint32) -> StateLabel
                  -> Ivory (AllocEffects s) ()
        nextstate newstate_emitter lbl@(StateLabel ns) = do
          -- The first use of nextstate in an event cycle will be the one to be
          -- accepted, since newstate_emitter comes from a channel of size 1.
          comment ("newstate " ++ statename lbl)
          emitV newstate_emitter (fromIntegral ns)
        mkState :: State -> Monitor e ()
        mkState (State s maybename handlers) = do
          ehs <- mapM mkHandler handlers
          handler newstate_out nsname $ do
            e <- emitter newstate_in 1
            callback $ \newstref -> do
              comment ("newstate handler for " ++ namecomment)
              newst <- deref newstref
              when (newst ==? (fromIntegral (unStateLabel s))) $ do
                store machine_state newst
                now <- getTime
                currenttime <- local (ival now)
                forM_ ehs $ \(ScopedStatements ss) ->
                  mapM_ (mkStmt e) (ss e (constRef currenttime))
          where
          namecomment = case maybename of
            Just n -> "state " ++ n
            Nothing -> "unnamed state " ++ show (unStateLabel s)
          nsname = case maybename of
            Just n -> "newstate_" ++ n
            Nothing -> "newstate"
          onEvt :: (IvoryArea area, IvoryZero area)
                => ChanOutput area
                -> Handler area e ()
                -> Monitor e ()
          onEvt e h = case maybename of
            Just n  -> handler e n h
            Nothing -> handler e "statemachineevent" h

          mkHandler :: StateHandler -> Monitor e (ScopedStatements (Stored ITime))
          mkHandler (EntryStateHandler stmts) =
            return $ scopedICallback $ \e starttimeRef ->
              case stmts of
                ScopedStatements ss -> do
                  comment ("entry handler for " ++ namecomment)
                  mapM_ (mkStmt e) (ss e starttimeRef)
          mkHandler (TimeoutStateHandler i stmts) = do
            due <- state (named ("timeoutDue" ++ show i))
            onEvt tick $ do
              e <- emitter newstate_in 1
              callback $ \currenttime -> handleState s $ do
                comment ("timeout " ++ show i ++ " handler for " ++ namecomment)
                now <- deref currenttime
                d <- deref due
                when ((d >? 0) .&& (d <=? now)) $ case stmts of
                  ScopedStatements ss -> do
                    mapM_ (mkStmt e) (ss e currenttime)
                    store due 0
            return $ scopedICallback $ \_ starttimeRef -> do
              t <- deref starttimeRef
              store due (t + (fromIntegral i))

          mkHandler (PeriodStateHandler i stmts) = do
            due <- state (named ("periodDue" ++ show i))
            onEvt tick $ do
              e <- emitter newstate_in 1
              callback $ \currenttime -> case stmts of
                ScopedStatements ss -> handleState s $ do
                  comment ("period " ++ show i ++ " handler for " ++ namecomment)
                  now <- deref currenttime
                  d   <- deref due
                  when (d <=? now) $ do
                    mapM_ (mkStmt e) (ss e currenttime)
                    store due (d + (fromIntegral i))
            return $ scopedICallback $ \_ starttimeRef -> do
              t <- deref starttimeRef
              store due (t + (fromIntegral i))

          mkHandler (ChanStateHandler evt stmts) = do
            onEvt evt $ do
              e <- emitter newstate_in 1
              callback $ case stmts of
                ScopedStatements ss -> \v -> handleState s $ do
                  comment ("event " ++ eventDescription evt ++ " handler for " ++ namecomment)
                  mapM_ (mkStmt e) (ss e v)
            return $ scopedICallback $ \_ _ -> return ()

        mkStmt :: Emitter (Stored Uint32) -> Stmt s -> Ivory (AllocEffects s) ()
        mkStmt e (Stmt s) = do
          cflowM <- s
          foldr (mkCFlow e) (return ()) (runCFlowM cflowM)

        mkCFlow :: Emitter (Stored Uint32) -> CFlowAST -> Ivory (AllocEffects s) ()
                -> Ivory (AllocEffects s) ()
        mkCFlow e (CFlowBranch b lbl) acc = ifte_ b (nextstate e lbl) acc
        mkCFlow _ (CFlowHalt   b)     acc = ifte_ b (store machine_active false) acc

        eventDescription _ = "???"

    mapM_ mkState states
    monitorModuleDef moddef
    return runner
    where
    named n = (showUnique uniq) ++ "_" ++ n

    machine_active_area = area (named "machine_active")    (Just (ival false))
    machine_state_area  = area (named "machine_state")     (Just (ival 0))

    (machine_active :: Ref Global (Stored IBool))  = addrOf machine_active_area
    (machine_state  :: Ref Global (Stored Uint32)) = addrOf machine_state_area

    active_proc = proc (named "active") $ body $
      deref machine_active >>= ret


    handleState :: StateLabel -> Ivory eff () -> Ivory eff ()
    handleState (StateLabel s) k = do
      current <- deref machine_state
      when (current ==? (fromIntegral s)) k

