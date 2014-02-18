{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Task
  ( Task
  , TaskCode(..)
  , runTask
  , putCommprim
  , putUsercode
  , putEvtEmitter
  , putEvtHandler
  , putChanReader
  , putPriority
  ) where

import MonadLib
import Control.Applicative (Applicative)

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Monad.Base

newtype Task a = Task
  { unTask :: StateT (AST.Task) TaskCodegen a
  } deriving (Functor, Monad, Applicative)


newtype TaskCodegen a = TaskCodegen
  { unTaskCodegen :: StateT (AST.System -> TaskCode) Base a
  } deriving (Functor, Monad, Applicative) 

data TaskCode =
  TaskCode
    { taskcode_commprim :: ModuleDef
    , taskcode_usercode :: ModuleDef
    }

runTask :: Task () -> Base (AST.Task, (AST.System -> TaskCode))
runTask t = do
  ((_,a),c) <- runTaskCodegen $ runStateT emptyast (unTask t)
  return (a,c)
  where
  runTaskCodegen :: TaskCodegen a -> Base (a, AST.System -> TaskCode)
  runTaskCodegen g = runStateT (const emptycode) (unTaskCodegen g)
  emptycode :: TaskCode
  emptycode = TaskCode
    { taskcode_commprim = return ()
    , taskcode_usercode = return ()
    }
  emptyast :: AST.Task
  emptyast = AST.Task
    { AST.task_evt_emitters = []
    , AST.task_evt_handlers = []
    , AST.task_chan_readers = []
    , AST.task_priority     = 0
    }

instance BaseUtils Task where
  getOS = Task $ lift $ TaskCodegen $ lift getOS
  fresh = Task $ lift $ TaskCodegen $ lift fresh
-- Internal API to TaskCodeGen

getTaskCode :: Task (AST.System -> TaskCode)
getTaskCode = Task (lift $ TaskCodegen get)

setTaskCode :: (AST.System -> TaskCode) -> Task ()
setTaskCode c = Task (lift $ TaskCodegen $ set c)

putCommprim :: (AST.System -> ModuleDef) -> Task ()
putCommprim p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_commprim =
                                    p sys >> taskcode_commprim (c sys) }

putUsercode :: (AST.System -> ModuleDef) -> Task ()
putUsercode p = do
  c <- getTaskCode
  setTaskCode $ \sys -> (c sys) { taskcode_usercode =
                                    p sys >> taskcode_usercode (c sys) }

-- Internal API to AST

getAST :: Task AST.Task
getAST = Task get

setAST :: AST.Task -> Task ()
setAST a = Task $ set a

putEvtEmitter :: AST.Chan -> Task ()
putEvtEmitter c = do
  a <- getAST
  setAST $ a { AST.task_evt_emitters = c : AST.task_evt_emitters a }

putEvtHandler :: AST.EventHandler -> Task ()
putEvtHandler e = do
  a <- getAST
  setAST $ a { AST.task_evt_handlers = e : AST.task_evt_handlers a }

putChanReader :: AST.ChanReader -> Task ()
putChanReader c = do
  a <- getAST
  setAST $ a { AST.task_chan_readers = c : AST.task_chan_readers a }

putPriority :: Integer -> Task ()
putPriority p = do
  a <- getAST
  setAST $ a { AST.task_priority = p }

