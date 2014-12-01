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

newtype MachineState = MachineState Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal )

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

mkCFlow :: CFlowM () -> Emitter (Stored MachineState)
          -> Ivory (AllocEffects s) ()
mkCFlow m e = foldr aux (return ()) (runCFlowM m)
  where
  aux (CFlowBranch b lbl) acc = ifte_ b (nextstate lbl) acc
  aux (CFlowHalt   b)     acc = ifte_ b (nextstate inactiveState) acc
  nextstate l = emitV e (stateLabel l)


data StateHandler e
  = EntryStateHandler                (StmtM (Stored MachineState) e ())
  | TimeoutStateHandler Microseconds (StmtM (Stored ITime) e ())
  | PeriodStateHandler Microseconds  (StmtM (Stored ITime) e ())
  | forall a . (IvoryArea a, IvoryZero a)
     => ChanStateHandler (ChanOutput a) (StmtM a e ())

instance Show (StateHandler e) where
  show (EntryStateHandler _)     = "EntryStateHandler"
  show (TimeoutStateHandler _ _) = "TimeoutStateHandler"
  show (PeriodStateHandler _ _)  = "PeriodStateHandler"
  show (ChanStateHandler _ _)    = "ChanStateHandler"

newtype ICflow = ICflow (forall eff . Ivory eff () -> Ivory eff ())

newtype StmtM t e a =
  StmtM
    { unStmtM :: ReaderT ( Emitter (Stored MachineState), ICflow)
                         (Handler t e) a
    } deriving (Functor, Applicative, Monad)

runStmtM :: StmtM t e ()
         -> Emitter (Stored MachineState)
         -> ICflow
         -> Handler t e ()
runStmtM m e r = runReaderT (e, r) (unStmtM m)

putStmtM :: (  Emitter (Stored MachineState)
            -> (forall eff . Ivory eff () -> Ivory eff ())
            -> Handler t e a)
         -> StmtM t e a
putStmtM k = StmtM $ do
  (e, (ICflow r)) <- ask
  lift (k e r)

machineControl :: (IvoryArea a)
               => (forall s s'. ConstRef s a -> Ivory (AllocEffects s') (CFlowM ()))
               -> StmtM a e ()
machineControl k = putStmtM $ \e r -> callback $ \a -> r $ do
    cfM <- k a
    mkCFlow cfM e

instance (IvoryArea a) => CFlowable (StmtM a e) where
  cflow a = machineControl (const (return (cflow a)))

data State e = State StateLabel (Maybe String) [StateHandler e]
     deriving (Show)

data StateLabel = StateLabel { unStateLabel :: Int }
     deriving (Eq, Show)

stateLabel :: StateLabel -> MachineState
stateLabel (StateLabel s) = MachineState (fromIntegral s)

newtype MachineM e a =
  MachineM
    { unMachineM :: WriterT [State e] (StateT StateLabel (Monitor e)) a
    } deriving (Functor, Monad, MonadFix, Applicative)

type Machine p = MachineM p StateLabel

newtype StateM e a =
  StateM
    { unStateM :: WriterT [StateHandler e] Id a
    } deriving (Functor, Monad, Applicative)

-- Machine monad specifies a set of states and gives the label for the initial state.
runMachineM :: MachineM e StateLabel
            -> Monitor e (StateLabel, [State e])
runMachineM sm = do
  ((istate, states), _) <- runStateT (StateLabel 1) (runWriterT (unMachineM sm))
  return (istate, states)

inactiveState :: StateLabel
inactiveState = StateLabel 0

label :: MachineM p StateLabel
label = MachineM get

writeState :: State e -> MachineM e ()
writeState sm = MachineM $ do
  put [sm]
  l <- get
  set (nextLabel l)
  where
  nextLabel :: StateLabel -> StateLabel
  nextLabel (StateLabel i) = StateLabel (i+1)

machineLocal :: (IvoryArea area, IvoryZero area)
             => String -> MachineM e (Ref Global area)
machineLocal n = MachineM $ lift $ lift $ state n

machineLocalInit :: (IvoryArea area)
                 => String -> Init area -> MachineM e (Ref Global area)
machineLocalInit n iv = MachineM $ lift $ lift $ stateInit n iv

writeStateHandler :: StateHandler e -> StateM e ()
writeStateHandler h = StateM $ put [h]

runStateM :: StateM e () -> StateLabel -> Maybe String -> State e
runStateM sh lbl n = State lbl n handlers
  where (_, handlers) = runM (unStateM sh)

