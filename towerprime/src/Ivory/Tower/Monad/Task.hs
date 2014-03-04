{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Task
  ( Task
  , TaskCode(..)
  , runTask
  , putCommprim
  , putUsercode
  , putChanEmitter
  , putChanReceiver
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
  { unTaskCodegen :: StateT (AST.System -> AST.Task -> TaskCode) Base a
  } deriving (Functor, Monad, Applicative) 

runTask :: Task p () -> Unique -> Base (AST.Task, (AST.System -> TaskCode))
runTask t n = do
  ((_,asttask),c) <- runTaskCodegen $ runStateT emptyast (unTask t)
  return (asttask,\sys -> c sys asttask)
  where
  runTaskCodegen g = runStateT (\_ _ -> emptycode) (unTaskCodegen g)
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
    { AST.task_name           = n
    , AST.task_chan_emitters  = []
    , AST.task_chan_receivers = []
    , AST.task_evts           = []
    , AST.task_evt_handlers   = []
    , AST.task_priority       = 0
    }

instance BaseUtils (Task p) where
  getOS = Task $ lift $ TaskCodegen $ lift getOS
  fresh = Task $ lift $ TaskCodegen $ lift fresh
-- Internal API to TaskCodeGen

getTaskCode :: Task p (AST.System -> AST.Task -> TaskCode)
getTaskCode = Task (lift $ TaskCodegen get)

setTaskCode :: (AST.System -> AST.Task -> TaskCode) -> Task p ()
setTaskCode c = Task (lift $ TaskCodegen $ set c)

putCommprim :: (AST.System -> AST.Task -> ModuleDef) -> Task p ()
putCommprim p = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_commprim =
                                    p sys t >> taskcode_commprim (c sys t) }

-- XXX - do we even need to expose the continuation here? Shouldn't this only
-- ever be "constant" usercode?
putUsercode :: (AST.System -> AST.Task -> ModuleDef) -> Task p ()
putUsercode p = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_usercode =
                                    p sys t >> taskcode_usercode (c sys t) }

putInitCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
            -> Task p ()
putInitCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_init =
                                    e sys t >> taskcode_init (c sys t) }

putTimerCode :: (forall s s2 . Ref s (Stored ITime) -> Ivory (AllocEffects s2) ())
                 -> Task p ()
putTimerCode e = do
  c <- getTaskCode
  setTaskCode $ \sys tsk -> (c sys tsk) { taskcode_timer = \time ->
                                    e time >> taskcode_timer (c sys tsk) time }

putEventReceiverCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventReceiverCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_eventrxer =
                                    e sys t >> taskcode_eventrxer (c sys t) }

putEventLoopCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
                 -> Task p ()
putEventLoopCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_eventloop =
                                    e sys t >> taskcode_eventloop (c sys t) }



-- Internal API to AST

getAST :: Task p AST.Task
getAST = Task get

setAST :: AST.Task -> Task p ()
setAST a = Task $ set a

putChanEmitter :: AST.ChanEmitter -> Task p ()
putChanEmitter c = do
  a <- getAST
  setAST $ a { AST.task_chan_emitters = c : AST.task_chan_emitters a }

putChanReceiver :: AST.ChanReceiver -> Task p ()
putChanReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_receivers = c : AST.task_chan_receivers a }

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

