{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monad where

import MonadLib

import Ivory.Language

import Ivory.Tower.Types

-- Monad Runners ---------------------------------------------------------------

runBase :: Base a -> OS -> a
runBase b os = fst (runM (unBase b) os 0)

runTower :: Tower () -> Base Assembly
runTower twr = do
  (_, towerst) <- runStateT emptyTowerSt (unTower twr)
  os <- getOS
  let tnodes  = towerst_tasknodes towerst
      snodes  = towerst_signodes towerst
      genTask = assembleTask os tnodes snodes
      genSig  = assembleSig  os tnodes snodes
  return $ Assembly { asm_towerst = towerst
                    , asm_tasks   = map genTask tnodes
                    , asm_sigs    = map genSig  snodes
                    , asm_system  = os_mkSysSchedule os tnodes snodes
                    }

assembleTask :: OS -> [TaskNode] -> [SigNode]
        -> TaskNode -> AssembledNode TaskSt
assembleTask os tnodes snodes tnode = AssembledNode
  { asmnode_nodest = tnode
  , asmnode_tldef  = mkbody sched
  , asmnode_moddef = taskst_moddef taskst sched
  }
  where
  sched  = os_mkTaskSchedule os tnodes snodes tnode
  taskst = nodest_impl tnode
  mkbody = case taskst_taskbody taskst of
    Just b -> b
    Nothing -> error ("tower input error: Missing task body in " 
                        ++ (nodest_name tnode))

assembleSig :: OS -> [TaskNode] -> [SigNode]
        -> SigNode -> AssembledNode SignalSt
assembleSig os tnodes snodes snode = AssembledNode
  { asmnode_nodest = snode
  , asmnode_tldef  = mkbody sched
  , asmnode_moddef = signalst_moddef sigst sched
  }
  where
  sched  = os_mkSigSchedule os tnodes snodes snode
  sigst  = nodest_impl snode
  mkbody = case signalst_body sigst of
    Just b -> b
    Nothing -> error ("tower input error: Missing signal body in "
                        ++ (nodest_name snode))

runTask :: Name -> Task () -> Tower TaskNode
runTask name t = do
  n <- freshname
  let uniquename = name ++ n
  (_, tnode) <- runStateT (emptyNodeSt uniquename emptyTaskSt) (unNode t)
  return $ tnode

runSignal :: Name -> Signal () -> Tower SigNode
runSignal name s = do
  n <- freshname
  let uniquename = name ++ n
  (_, snode) <- runStateT (emptyNodeSt uniquename emptySignalSt) (unNode s)
  return $ snode

-- Node Transformers----------------------------------------------------------

nodeStAddReceiver :: ChannelId -> String -> Node i ()
nodeStAddReceiver r lbl = do
  n <- getNode
  setNode $ n { nodest_receivers = (Labeled r lbl) : (nodest_receivers n)}

nodeStAddEmitter :: ChannelId -> String -> Node i ()
nodeStAddEmitter r lbl = do
  n <- getNode
  setNode $ n { nodest_emitters = (Labeled r lbl) : (nodest_emitters n)}

nodeStAddDataReader :: DataportId -> String -> Node i ()
nodeStAddDataReader cc lbl = do
  n <- getNode
  setNode $ n { nodest_datareaders = (Labeled cc lbl) : (nodest_datareaders n)}

nodeStAddDataWriter :: DataportId -> String -> Node i ()
nodeStAddDataWriter cc lbl = do
  n <- getNode
  setNode $ n { nodest_datawriters = (Labeled cc lbl) : (nodest_datawriters n)}

nodeStAddCodegen :: Def('[]:->()) -> ModuleDef -> Node i ()
nodeStAddCodegen i m = do
  n <- getNode
  setNode $ n { nodest_codegen = (NodeCodegen i m):(nodest_codegen n) }

getNode :: Node i (NodeSt i)
getNode = Node get

setNode :: NodeSt i -> Node i ()
setNode n = Node $ set n

getNodeName:: Node i Name
getNodeName= do
  n <- getNode
  return (nodest_name n)


-- Task Getters/Setters --------------------------------------------------------

getTaskSt :: Task TaskSt
getTaskSt = getNode >>= \n -> return (nodest_impl n)

setTaskSt :: TaskSt -> Task ()
setTaskSt s = getNode >>= \n -> setNode (n { nodest_impl = s })

taskStAddModuleDef :: (TaskSchedule -> ModuleDef) -> Task ()
taskStAddModuleDef md = do
  s <- getTaskSt
  setTaskSt s { taskst_moddef = \sch -> taskst_moddef s sch >> md sch }

-- Signal Getters/Setters --------------------------------------------------------

getSignalSt :: Signal SignalSt
getSignalSt = getNode >>= \n -> return (nodest_impl n)

setSignalSt :: SignalSt -> Signal ()
setSignalSt s = getNode >>= \n -> setNode (n { nodest_impl = s })

-- Tower Getters/Setters -------------------------------------------------------

getTowerSt :: Tower TowerSt
getTowerSt = Tower get

setTowerSt :: TowerSt -> Tower ()
setTowerSt s = Tower $ set s

addTaskNode :: TaskNode -> Tower ()
addTaskNode n = do
  s <- getTowerSt
  setTowerSt $ s { towerst_tasknodes = n : (towerst_tasknodes s) }

addSigNode :: SigNode -> Tower ()
addSigNode n = do
  s <- getTowerSt
  setTowerSt $ s { towerst_signodes = n : (towerst_signodes s) }

