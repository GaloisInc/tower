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

data Handler
  = EntryHandler (ScopedStatements (Stored ITime))
  | TimeoutHandler Micros (ScopedStatements (Stored ITime))
  | PeriodHandler Micros (ScopedStatements (Stored ITime))
  | forall a . (IvoryArea a, IvoryZero a)
     => EventHandler (Event a) (ScopedStatements a)

instance Show Handler where
  show (EntryHandler _)     = "EntryHandler"
  show (TimeoutHandler _ _) = "TimeoutHandler"
  show (PeriodHandler _ _)  = "PeriodHandler"
  show (EventHandler _ _)   = "EventHandler"

data ScopedStatements a = ScopedStatements (forall s s' . ConstRef s' a -> [Stmt s])

scopedIvory :: (forall s s' . ConstRef s' a -> Ivory (AllocEffects s) ())
            -> ScopedStatements a
scopedIvory k = ScopedStatements (\r -> [ Stmt ( k r >> return (return ())) ] )


data State = State StateLabel (Maybe String) [Handler]
     deriving (Show)

data StateLabel = StateLabel { unStateLabel :: Int }
     deriving (Eq, Show)

newtype MachineM a =
  MachineM
    { unMachineM :: WriterT [State] (StateT StateLabel Id) a
    } deriving (Functor, Monad, MonadFix, Applicative)

type Machine = MachineM StateLabel

newtype StateM a =
  StateM 
    { unStateM :: WriterT [Handler] Id a
    } deriving (Functor, Monad, Applicative)

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

runStateM :: StateM () -> StateLabel -> Maybe String -> State
runStateM sh lbl n = State lbl n handlers
  where (_, handlers) = runM (unStateM sh)

