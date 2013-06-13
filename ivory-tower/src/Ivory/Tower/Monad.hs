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
  let tnodes = towerst_tasknodes towerst
      snodes = towerst_signodes towerst

      genTask :: TaskNode -> (Def('[]:->()), ModuleDef)
      genTask tnode = (scheduleTaskBody tsch, (taskst_moddef aTask) tsch)
        where
        aTask = nodest_impl tnode
        tsch = os_mkTaskSchedule os tnodes snodes tnode
        scheduleTaskBody = case taskst_taskbody aTask of
          Just b -> b
          Nothing -> error ("runTower input Error: Missing task body in " 
                            ++ (nodest_name tnode))

  return $ Assembly { asm_towerst = towerst
                    , asm_taskdefs = let res = map genTask tnodes in
                                     let (defs, mods) = unzip res in
                                     zip3 tnodes defs mods
                    , asm_system  = os_mkSysSchedule os tnodes snodes
                    }

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

-- XXX put these back in Node i monad
nodeStAddReceiver :: ChannelId -> String -> NodeSt a -> NodeSt a
nodeStAddReceiver r lbl s =
  s { nodest_receivers = (Labeled r lbl) : (nodest_receivers s)}

nodeStAddEmitter :: ChannelId -> String -> NodeSt a -> NodeSt a
nodeStAddEmitter r lbl s =
  s { nodest_emitters = (Labeled r lbl) : (nodest_emitters s)}

nodeStAddDataReader :: DataportId -> String -> NodeSt a -> NodeSt a
nodeStAddDataReader cc lbl s =
  s { nodest_datareaders = (Labeled cc lbl) : (nodest_datareaders s)}

nodeStAddDataWriter :: DataportId -> String -> NodeSt a -> NodeSt a
nodeStAddDataWriter cc lbl s =
  s { nodest_datawriters = (Labeled cc lbl) : (nodest_datawriters s)}

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

taskStAddChannelInit :: Def('[]:->()) -> Task ()
taskStAddChannelInit ci = do
  s <- getTaskSt
  setTaskSt $ s { taskst_channelinit = ci : (taskst_channelinit s) }

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

