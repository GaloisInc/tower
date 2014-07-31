{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Task
  ( Task
  , TaskCode(..)
  , runTask
  , getTaskName
  , putCommprim
  , putUsercode
  , putChanEventReceiver
  , putChanReader
  , putSignalReceiver
  , putChanPollReceiver
  , putASTEvent
  , putASTHandler
  , putUserInitCode
  , putSysInitCode
  , putTimerCode
  , putEventReceiverCode
  , putEventLoopCode
  , putPriority
  , putStackSize
  , taskLiftTower
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative (Applicative)

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.TaskCode
import Ivory.Tower.Types.Signalable
import Ivory.Tower.Types.Unique

newtype Task p a = Task
  { unTask :: StateT (AST.Task p) (TaskCodegen p) a
  } deriving (Functor, Monad, Applicative, MonadFix)


newtype TaskCodegen p a = TaskCodegen
  { unTaskCodegen :: StateT (AST.System p -> TaskCode) (Tower p) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTask :: Task p ()
        -> Unique
        -> Tower p (AST.Task p, (AST.System p -> TaskCode))
runTask t n = do
  ((_,asttask),c) <- runTaskCodegen $ runStateT emptyast (unTask t)
  return (asttask, c)
  where
  runTaskCodegen g = runStateT (\_ -> emptycode) (unTaskCodegen g)
  emptycode :: TaskCode
  emptycode = TaskCode
    { taskcode_taskname  = n
    , taskcode_commprim  = return ()
    , taskcode_usercode  = return ()
    , taskcode_user_init = return ()
    , taskcode_sys_init = return ()
    , taskcode_timer     = const (return ())
    , taskcode_eventrxer = return ()
    , taskcode_eventloop = return ()
    }
  emptyast :: AST.Task p
  emptyast = AST.Task
    { AST.task_name                 = n
    , AST.task_chan_poll_receivers  = []
    , AST.task_chan_event_receivers = []
    , AST.task_chan_readers         = []
    , AST.task_signal_receivers     = []
    , AST.task_evts                 = []
    , AST.task_handlers             = []
    , AST.task_priority             = 0
    , AST.task_stack_size           = 1024
    }

instance BaseUtils (Task p) where
  getOS = taskLiftTower getOS
  fresh = taskLiftTower fresh

taskLiftTower :: Tower p a -> Task p a
taskLiftTower k = Task $ lift $ TaskCodegen $ lift k

-- Internal API to TaskCodeGen

getTaskCode :: Task p (AST.System p -> TaskCode)
getTaskCode = Task (lift $ TaskCodegen get)

setTaskCode :: (AST.System p -> TaskCode) -> Task p ()
setTaskCode c = Task (lift $ TaskCodegen $ set c)

putCommprim :: (AST.System p -> ModuleDef) -> Task p ()
putCommprim p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_commprim =
                                    p sys >> taskcode_commprim (c sys) }

putUsercode :: ModuleDef -> Task p ()
putUsercode p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_usercode =
                                    p >> taskcode_usercode (c sys) }

putUserInitCode :: (forall s . AST.System p -> Ivory (AllocEffects s) ())
            -> Task p ()
putUserInitCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_user_init =
                                    taskcode_user_init (c sys) >> e sys }

putSysInitCode :: (forall s . AST.System p -> Ivory (AllocEffects s) ())
            -> Task p ()
putSysInitCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_sys_init =
                                    taskcode_sys_init (c sys) >> e sys }

putTimerCode :: (forall s s2 . Ref s (Stored ITime) -> Ivory (AllocEffects s2) ())
                 -> Task p ()
putTimerCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_timer = \time ->
                                    e time >> taskcode_timer (c sys) time }

putEventReceiverCode :: (forall s . AST.System p -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventReceiverCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_eventrxer =
                                    e sys >> taskcode_eventrxer (c sys) }

putEventLoopCode :: (forall s . AST.System p -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventLoopCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_eventloop =
                                    e sys >> taskcode_eventloop (c sys) }



-- Internal API to AST

getAST :: Task p (AST.Task p)
getAST = Task get

setAST :: AST.Task p -> Task p ()
setAST a = Task $ set a

getTaskName :: Task p Unique
getTaskName = do
  a <- getAST
  return (AST.task_name a)

putChanPollReceiver :: AST.ChanReceiver -> Task p ()
putChanPollReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_poll_receivers = c : AST.task_chan_poll_receivers a }

putChanEventReceiver :: AST.ChanReceiver -> Task p ()
putChanEventReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_event_receivers = c : AST.task_chan_event_receivers a }

putChanReader :: AST.ChanReader -> Task p ()
putChanReader c = do
  a <- getAST
  setAST $ a { AST.task_chan_readers = c : AST.task_chan_readers a }

putSignalReceiver :: AST.SignalReceiver (SignalType p) -> Task p ()
putSignalReceiver s = do
  a <- getAST
  setAST $ a { AST.task_signal_receivers = s : AST.task_signal_receivers a }

putASTEvent :: AST.Event -> Task p ()
putASTEvent e = do
  a <- getAST
  setAST $ a { AST.task_evts = e : AST.task_evts a }

putASTHandler :: AST.Handler -> Task p ()
putASTHandler e = do
  a <- getAST
  setAST $ a { AST.task_handlers = e : AST.task_handlers a }

putPriority :: Integer -> Task p ()
putPriority p = do
  a <- getAST
  setAST $ a { AST.task_priority = max p (AST.task_priority a) }

putStackSize :: Integer -> Task p ()
putStackSize p = do
  a <- getAST
  setAST $ a { AST.task_stack_size = max p (AST.task_stack_size a) }

