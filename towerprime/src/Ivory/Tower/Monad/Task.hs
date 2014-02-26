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

newtype Task a = Task
  { unTask :: StateT (AST.Task) TaskCodegen a
  } deriving (Functor, Monad, Applicative)


newtype TaskCodegen a = TaskCodegen
  { unTaskCodegen :: StateT (AST.System -> AST.Task -> TaskCode) Base a
  } deriving (Functor, Monad, Applicative) 

data TaskCode =
  TaskCode
    { taskcode_commprim  :: ModuleDef
    , taskcode_usercode  :: ModuleDef
    , taskcode_init      :: forall s . Ivory (AllocEffects s) ()
    , taskcode_timer     :: forall s . Ref (Stack s) (Stored ITime)
                                    -> Ivory (AllocEffects s) ()
    , taskcode_eventrxer :: forall s . Ivory (AllocEffects s) ()
    , taskcode_eventloop :: forall s . Ivory (AllocEffects s) ()
    }

runTask :: Task () -> Base (AST.Task, (AST.System -> TaskCode))
runTask t = do
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
    { AST.task_chan_emitters  = []
    , AST.task_chan_receivers = []
    , AST.task_evts           = []
    , AST.task_evt_handlers   = []
    , AST.task_priority       = 0
    }

instance BaseUtils Task where
  getOS = Task $ lift $ TaskCodegen $ lift getOS
  fresh = Task $ lift $ TaskCodegen $ lift fresh
-- Internal API to TaskCodeGen

getTaskCode :: Task (AST.System -> AST.Task -> TaskCode)
getTaskCode = Task (lift $ TaskCodegen get)

setTaskCode :: (AST.System -> AST.Task -> TaskCode) -> Task ()
setTaskCode c = Task (lift $ TaskCodegen $ set c)

putCommprim :: (AST.System -> AST.Task -> ModuleDef) -> Task ()
putCommprim p = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_commprim =
                                    p sys t >> taskcode_commprim (c sys t) }

-- XXX - do we even need to expose the continuation here? Shouldn't this only
-- ever be "constant" usercode?
putUsercode :: (AST.System -> AST.Task -> ModuleDef) -> Task ()
putUsercode p = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_usercode =
                                    p sys t >> taskcode_usercode (c sys t) }

putInitCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
            -> Task ()
putInitCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_init =
                                    e sys t >> taskcode_init (c sys t) }

putTimerCode :: (forall s . Ref (Stack s) (Stored ITime) -> Ivory (AllocEffects s) ())
                 -> Task ()
putTimerCode e = do
  c <- getTaskCode
  setTaskCode $ \sys tsk -> (c sys tsk) { taskcode_timer = \time ->
                                    e time >> taskcode_timer (c sys tsk) time }

putEventReceiverCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
                 -> Task ()
putEventReceiverCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_eventrxer =
                                    e sys t >> taskcode_eventrxer (c sys t) }

putEventLoopCode :: (forall s . AST.System -> AST.Task -> Ivory (AllocEffects s) ())
                 -> Task ()
putEventLoopCode e = do
  c <- getTaskCode
  setTaskCode $ \sys t -> (c sys t) { taskcode_eventloop =
                                    e sys t >> taskcode_eventloop (c sys t) }



-- Internal API to AST

getAST :: Task AST.Task
getAST = Task get

setAST :: AST.Task -> Task ()
setAST a = Task $ set a

putChanEmitter :: AST.ChanEmitter -> Task ()
putChanEmitter c = do
  a <- getAST
  setAST $ a { AST.task_chan_emitters = c : AST.task_chan_emitters a }

putChanReceiver :: AST.ChanReceiver -> Task ()
putChanReceiver c = do
  a <- getAST
  setAST $ a { AST.task_chan_receivers = c : AST.task_chan_receivers a }

putASTEvent :: AST.Event -> Task ()
putASTEvent e = do
  a <- getAST
  setAST $ a { AST.task_evts = e : AST.task_evts a }

putASTEventHandler :: AST.EventHandler -> Task ()
putASTEventHandler e = do
  a <- getAST
  setAST $ a { AST.task_evt_handlers = e : AST.task_evt_handlers a }

putPriority :: Integer -> Task ()
putPriority p = do
  a <- getAST
  setAST $ a { AST.task_priority = p }

