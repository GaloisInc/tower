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
  , putChanEmitter
  , putChanEventReceiver
  , putChanPollReceiver
  , putASTEvent
  , putASTEventHandler
  , putInitCode
  , putTimerCode
  , putEventReceiverCode
  , putEventLoopCode
  , putPriority
  ) where

import MonadLib
import Control.Applicative (Applicative)

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.TaskCode
import Ivory.Tower.Types.Unique

newtype Task p a = Task
  { unTask :: StateT (AST.Task) TaskCodegen a
  } deriving (Functor, Monad, Applicative)


newtype TaskCodegen a = TaskCodegen
  { unTaskCodegen :: StateT (AST.System -> TaskCode) Base a
  } deriving (Functor, Monad, Applicative) 

runTask :: Task p () -> Unique -> Base (AST.Task, (AST.System -> TaskCode))
runTask t n = do
  ((_,asttask),c) <- runTaskCodegen $ runStateT emptyast (unTask t)
  return (asttask, c)
  where
  runTaskCodegen g = runStateT (\_ -> emptycode) (unTaskCodegen g)
  emptycode :: TaskCode
  emptycode = TaskCode
    { taskcode_commprim  = return ()
    , taskcode_usercode  = return ()
    , taskcode_init      = return ()
    , taskcode_timer     = const (return ())
    , taskcode_eventrxer = return ()
    , taskcode_eventloop = return ()
    }
  emptyast :: AST.Task
  emptyast = AST.Task
    { AST.task_name                 = n
    , AST.task_chan_emitters        = []
    , AST.task_chan_poll_receivers  = []
    , AST.task_chan_event_receivers = []
    , AST.task_evts                 = []
    , AST.task_evt_handlers         = []
    , AST.task_priority             = 0
    }

instance BaseUtils (Task p) where
  getOS = Task $ lift $ TaskCodegen $ lift getOS
  fresh = Task $ lift $ TaskCodegen $ lift fresh
-- Internal API to TaskCodeGen

getTaskCode :: Task p (AST.System -> TaskCode)
getTaskCode = Task (lift $ TaskCodegen get)

setTaskCode :: (AST.System -> TaskCode) -> Task p ()
setTaskCode c = Task (lift $ TaskCodegen $ set c)

putCommprim :: (AST.System -> ModuleDef) -> Task p ()
putCommprim p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_commprim =
                                    p sys >> taskcode_commprim (c sys) }

putUsercode :: ModuleDef -> Task p ()
putUsercode p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_usercode =
                                    p >> taskcode_usercode (c sys) }

putInitCode :: (forall s . AST.System -> Ivory (AllocEffects s) ())
            -> Task p ()
putInitCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_init =
                                    e sys >> taskcode_init (c sys) }

putTimerCode :: (forall s s2 . Ref s (Stored ITime) -> Ivory (AllocEffects s2) ())
                 -> Task p ()
putTimerCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_timer = \time ->
                                    e time >> taskcode_timer (c sys) time }

putEventReceiverCode :: (forall s . AST.System -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventReceiverCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_eventrxer =
                                    e sys >> taskcode_eventrxer (c sys) }

putEventLoopCode :: (forall s . AST.System -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventLoopCode e = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_eventloop =
                                    e sys >> taskcode_eventloop (c sys) }



-- Internal API to AST

getAST :: Task p AST.Task
getAST = Task get

setAST :: AST.Task -> Task p ()
setAST a = Task $ set a

getTaskName :: Task p Unique
getTaskName = do
  a <- getAST
  return (AST.task_name a)

putChanEmitter :: AST.ChanEmitter -> Task p ()
putChanEmitter c = do
  a <- getAST
  setAST $ a { AST.task_chan_emitters = c : AST.task_chan_emitters a }

putChanPollReceiver :: AST.ChanReceiver -> Task p ()
putChanPollReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_poll_receivers = c : AST.task_chan_poll_receivers a }

putChanEventReceiver :: AST.ChanReceiver -> Task p ()
putChanEventReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_event_receivers = c : AST.task_chan_event_receivers a }

putASTEvent :: AST.Event -> Task p ()
putASTEvent e = do
  a <- getAST
  setAST $ a { AST.task_evts = e : AST.task_evts a }

putASTEventHandler :: AST.EventHandler -> Task p ()
putASTEventHandler e = do
  a <- getAST
  setAST $ a { AST.task_evt_handlers = e : AST.task_evt_handlers a }

putPriority :: Integer -> Task p ()
putPriority p = do
  a <- getAST
  setAST $ a { AST.task_priority = p }

