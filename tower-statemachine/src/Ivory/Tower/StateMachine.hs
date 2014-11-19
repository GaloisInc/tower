{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.StateMachine
  ( on
  , periodic
  , timeout
  , entry

  , Runnable
  , active
  , begin
  , stateMachine

  , MachineM
  , Stmt
  , StateLabel

  , machineState
  , machineStateNamed
  , branch
  , goto
  , haltWhen
  , halt
  , liftIvory
  , liftIvory_
  , machineLocal
  , machineLocalInit

  , unScopedIvory
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine.Monad
import Ivory.Tower.StateMachine.Compile

on :: forall area
         . (IvoryArea area, IvoryZero area)
        => ChanOutput area
        -> (forall s s' . ConstRef s' area -> StmtM s ())
        -> StateM ()
on c stmtM = writeStateHandler
           $ ChanStateHandler c (ScopedStatements stmts)
  where
  stmts :: Emitter a -> ConstRef s' area -> [Stmt s]
  stmts _ ref = runStmtM (stmtM ref)

entry :: (forall s . StmtM s ()) -> StateM ()
entry stmtM = writeStateHandler
            $ EntryStateHandler
            $ ScopedStatements (\_ _ -> runStmtM stmtM)

timeout :: Time a => a -> (forall s . StmtM s ()) -> StateM ()
timeout t stmtM = writeStateHandler
                $ TimeoutStateHandler (toMicroseconds t)
                $ ScopedStatements (\_ _ -> runStmtM stmtM)

periodic :: Time a => a -> (forall s . StmtM s ()) -> StateM ()
periodic t stmtM = writeStateHandler
                 $ PeriodStateHandler (toMicroseconds t)
                 $ ScopedStatements (\ _ _ -> runStmtM stmtM)

machineState :: StateM () -> MachineM p StateLabel
machineState sh = stateAux sh Nothing

machineStateNamed :: String -> StateM () -> MachineM p StateLabel
machineStateNamed n sh = stateAux sh (Just n)

stateAux :: StateM () -> Maybe String-> MachineM p StateLabel
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

liftIvory_ :: Ivory (AllocEffects s) () -> StmtM s ()
liftIvory_ i = writeStmt $ Stmt (i >> return (return ()))

liftIvory :: Ivory (AllocEffects s) CFlow-> StmtM s ()
liftIvory i = writeStmt $ Stmt i

