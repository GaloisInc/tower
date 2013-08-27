{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.StateMachine.Types where

import Control.Monad.Fix
import MonadLib hiding (StateM)

import Ivory.Language
import Ivory.Tower.Types

data CFlowAST = CFlowBranch IBool StateLabel
              | CFlowHalt   IBool

newtype CFlowM a =
  CFlowM
    { unCFlowM :: WriterT [CFlowAST] Id a
    } deriving (Functor, Monad)

type CFlow = CFlowM ()

writeCFlow :: CFlowAST -> CFlowM ()
writeCFlow s = CFlowM$ put [s]

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
    } deriving (Functor, Monad)

writeStmt :: Stmt s -> StmtM s ()
writeStmt s = StmtM $ put [s]

runStmtM :: StmtM s () -> [Stmt s]
runStmtM m = ss
  where (_, ss) = runM (unStmtM m)

instance CFlowable (StmtM s) where
  cflow cf = writeStmt (Stmt (return (writeCFlow cf)))

data Handler
  = TimeoutHandler Int (ScopedStatements (Stored Uint32))
  | forall a . (IvoryArea a, IvoryZero a)
     => EventHandler (Event a) (ScopedStatements a)

instance Show Handler where
  show (TimeoutHandler _ _) = "TimeoutHandler"
  show (EventHandler _ _) = "EventHandler"

data ScopedStatements a = ScopedStatements (forall s s' . ConstRef s' a -> [Stmt s])

data State = State StateLabel [Handler]
     deriving (Show)

data StateLabel = StateLabel Int
     deriving (Show)

newtype MachineM a =
  MachineM
    { unMachineM :: WriterT [State] (StateT StateLabel Id) a
    } deriving (Functor, Monad, MonadFix)

type Machine = MachineM StateLabel

newtype StateM a =
  StateM 
    { unStateM :: WriterT [Handler] Id a
    } deriving (Functor, Monad)

runMachineM :: MachineM StateLabel -> (StateLabel, [State])
runMachineM sm = (istate, states)
  where ((istate, states), _) = runM (unMachineM sm) (StateLabel 0)

label :: MachineM StateLabel
label = MachineM get

writeState :: State -> MachineM ()
writeState sm = MachineM $ do
  put [sm]
  l <- get
  set (nextLabel l)
  where
  nextLabel :: StateLabel -> StateLabel
  nextLabel (StateLabel i) = StateLabel (i+1)

writeHandler :: Handler -> StateM ()
writeHandler h = StateM $ put [h]

runStateM :: StateM () -> StateLabel -> State
runStateM sh lbl = State lbl handlers
  where (_, handlers) = runM (unStateM sh)

