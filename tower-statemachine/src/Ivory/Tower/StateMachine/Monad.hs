{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.StateMachine.Monad where

import Control.Applicative
import Control.Monad.Fix
import MonadLib hiding (StateM)

import Ivory.Language
import Ivory.Tower

data CFlowAST = CFlowBranch IBool StateLabel
              | CFlowHalt   IBool

newtype CFlowM a =
  CFlowM
    { unCFlowM :: WriterT [CFlowAST] Id a
    } deriving (Functor, Monad, Applicative)

type CFlow = CFlowM ()

writeCFlow :: CFlowAST -> CFlowM ()
writeCFlow s = CFlowM $ put [s]

runCFlowM :: CFlowM () -> [CFlowAST]
runCFlowM m = snd $ runM (unCFlowM m)

class CFlowable m where
  cflow :: CFlowAST -> m ()

instance CFlowable CFlowM where
  cflow = writeCFlow

data Stmt s = Stmt (Ivory (AllocEffects s) CFlow)

newtype StmtM s a =
  StmtM
    { unStmtM :: WriterT [Stmt s] Id a
    } deriving (Functor, Monad, Applicative)

writeStmt :: Stmt s -> StmtM s ()
writeStmt s = StmtM $ put [s]

runStmtM :: StmtM s () -> [Stmt s]
runStmtM m = ss
  where (_, ss) = runM (unStmtM m)

instance CFlowable (StmtM s) where
  cflow cf = writeStmt (Stmt (return (writeCFlow cf)))

type Micros = Integer

data StateHandler
  = EntryStateHandler (ScopedStatements (Stored ITime))
  | TimeoutStateHandler Micros (ScopedStatements (Stored ITime))
  | PeriodStateHandler Micros (ScopedStatements (Stored ITime))
  | forall a . (IvoryArea a, IvoryZero a)
     => EventStateHandler (Event a) (ScopedStatements a)

instance Show StateHandler where
  show (EntryStateHandler _)     = "EntryStateHandler"
  show (TimeoutStateHandler _ _) = "TimeoutStateHandler"
  show (PeriodStateHandler _ _)  = "PeriodStateHandler"
  show (EventStateHandler _ _)   = "EventStateHandler"

data ScopedStatements a = ScopedStatements (forall s s' . ConstRef s' a -> [Stmt s])

scopedIvory :: (forall s s' . ConstRef s' a -> Ivory (AllocEffects s) ())
            -> ScopedStatements a
scopedIvory k = ScopedStatements (\r -> [ Stmt ( k r >> return (return ())) ] )


data State = State StateLabel (Maybe String) [StateHandler]
     deriving (Show)

data StateLabel = StateLabel { unStateLabel :: Int }
     deriving (Eq, Show)

newtype MachineM p a =
  MachineM
    { unMachineM :: WriterT [State] (StateT StateLabel (Task p)) a
    } deriving (Functor, Monad, MonadFix, Applicative)

type Machine p = MachineM p StateLabel

newtype StateM a =
  StateM
    { unStateM :: WriterT [StateHandler] Id a
    } deriving (Functor, Monad, Applicative)

runMachineM :: MachineM p StateLabel -> Task p (StateLabel, [State])
runMachineM sm = do
  ((istate, states), _) <- runStateT (StateLabel 0) (runWriterT (unMachineM sm))
  return (istate, states)

label :: MachineM p StateLabel
label = MachineM get

writeState :: State -> MachineM p ()
writeState sm = MachineM $ do
  put [sm]
  l <- get
  set (nextLabel l)
  where
  nextLabel :: StateLabel -> StateLabel
  nextLabel (StateLabel i) = StateLabel (i+1)

machineLocal :: (IvoryArea area, IvoryZero area)
             => String -> MachineM p (Ref Global area)
machineLocal n = MachineM $ lift $ lift $ taskLocal n

machineLocalInit :: (IvoryArea area)
                 => String -> Init area -> MachineM p (Ref Global area)
machineLocalInit n iv = MachineM $ lift $ lift $ taskLocalInit n iv

writeStateHandler :: StateHandler -> StateM ()
writeStateHandler h = StateM $ put [h]

runStateM :: StateM () -> StateLabel -> Maybe String -> State
runStateM sh lbl n = State lbl n handlers
  where (_, handlers) = runM (unStateM sh)

