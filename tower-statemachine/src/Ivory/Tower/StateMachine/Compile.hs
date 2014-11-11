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
--import Ivory.Tower.Types.Event (eventDescription)
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

stateMachine :: forall p . String -> MachineM p StateLabel -> Task p Runnable
stateMachine name machine = do
  uniq <- freshname ("machine_" ++ name)
  tick <- withPeriodicEvent (Milliseconds 1)
  newstate <- taskChannel' (Proxy :: Proxy 2) Nothing
  newstate_emitter <- withChannelEmitter (src newstate) "newstateEmitter"
  newstate_receiver <- withChannelEvent (snk newstate) "newstateEvent"
  aux uniq tick newstate_emitter newstate_receiver
  where
  aux :: Unique
      -> Event (Stored ITime)
      -> ChannelEmitter (Stored Uint32)
      -> Event (Stored Uint32)
      -> Task p Runnable
  aux uniq tick newstate_emitter newstate_evt = do
    (istate, states) <- runMachineM machine
    let begin_proc = proc (named "begin") $ body $ do
          a <- deref activeState
          unless a $ noReturn $ do
            store activeState true
            nextstate istate

        statename :: StateLabel -> String
        statename lbl = case find aux' states of
          Just (State _ (Just n) _) -> "state " ++ n
          _ -> "unnamed state #" ++ show (unStateLabel lbl)
          where
          aux' (State l _ _) = l == lbl
        runner = Runnable
          { runnable_begin = begin_proc
          , runnable_active = active_proc
          }
        moddef = do
          incl begin_proc
          incl active_proc
          defMemArea active_area
          defMemArea state_area

        nextstate :: StateLabel -> Ivory (AllocEffects s) ()
        nextstate lbl@(StateLabel ns) = do
          -- The first use of nextstate in an event cycle will be the one to be
          -- accepted, since newstate_emitter comes from a channel of size 1.
          comment ("newstate " ++ statename lbl)
          emitV_ newstate_emitter (fromIntegral ns)
        mkState :: State -> Task p ()
        mkState (State s maybename handlers) = do
          ehs <- mapM mkHandler handlers
          handle newstate_evt nsname $ \newstref -> noReturn $ do
            comment ("newstate handler for " ++ namecomment)
            newst <- deref newstref
            when (newst ==? (fromIntegral (unStateLabel s))) $ do
              store state newst
              now <- getTime
              currenttime <- local (ival now)
              forM_ ehs $ \(ScopedStatements ss) ->
                mapM_ mkStmt (ss (constRef currenttime))
          where
          namecomment = case maybename of
            Just n -> "state " ++ n
            Nothing -> "unnamed state " ++ show (unStateLabel s)
          nsname = case maybename of
            Just n -> "newstate_" ++ n
            Nothing -> "newstate"
          onEvt :: (IvoryArea area, IvoryZero area)
                => Event area
                -> (forall s s' . ConstRef s area -> Ivory (ProcEffects s' ()) ())
                -> Task p ()
          onEvt e k = case maybename of
            Just n  -> handle e n k
            Nothing -> handle e "statemachineevent" k
          mkHandler (EntryHandler stmts) = return $ scopedIvory $ \starttimeRef ->
            case stmts of
              ScopedStatements ss -> do
                comment ("entry handler for " ++ namecomment)
                mapM_ mkStmt (ss starttimeRef)
          mkHandler (TimeoutHandler i stmts) = do
            due <- taskLocal (named ("timeoutDue" ++ show i))
            onEvt tick $ \currenttime -> handleState s $ do
              comment ("timeout " ++ show i ++ " handler for " ++ namecomment)
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
            due <- taskLocal (named ("periodDue" ++ show i))
            onEvt tick $ \currenttime -> case stmts of
              ScopedStatements ss -> handleState s $ do
                comment ("period " ++ show i ++ " handler for " ++ namecomment)
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
                comment ("event " ++ eventDescription evt ++ " handler for " ++ namecomment)
                mapM_ mkStmt (ss v)
            return $ scopedIvory $ const (return ())

        mkStmt :: Stmt s -> Ivory (AllocEffects s) ()
        mkStmt (Stmt s) = do
          cflowM <- s
          foldr mkCFlow (return ()) (runCFlowM cflowM)

        mkCFlow :: CFlowAST -> Ivory (AllocEffects s) () -> Ivory (AllocEffects s) ()
        mkCFlow (CFlowBranch b lbl) acc = ifte_ b (nextstate lbl) acc
        mkCFlow (CFlowHalt   b)     acc = ifte_ b (store activeState false) acc


    mapM_ mkState states
    taskModuleDef moddef
    return runner
    where
    named n = (showUnique uniq) ++ "_" ++ n

    active_area = area (named "state_active")      (Just (ival false))
    state_area  = area (named "state")             (Just (ival 0))

    (activeState     :: Ref Global (Stored IBool))  = addrOf active_area
    (state           :: Ref Global (Stored Uint32)) = addrOf state_area

    active_proc = proc (named "active") $ body $
      deref activeState >>= ret


    handleState :: StateLabel -> Ivory (AllocEffects s) () -> Ivory (ProcEffects s ()) ()
    handleState (StateLabel s) k = do
      current <- deref state
      when (current ==? (fromIntegral s)) (noReturn k)

