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

      generate :: TaskNode -> (Def('[]:->()), ModuleDef)
      generate tnode = (scheduleTaskBody sch, (taskst_moddef aTask) sch)
        where
        aTask = nodest_impl tnode
        sch = os_mkTaskSchedule os tnodes tnode
        scheduleTaskBody = case taskst_taskbody aTask of
          Just b -> b
          Nothing -> error ("runTower input Error: Missing task body in " 
                            ++ (nodest_name tnode))

  return $ Assembly { asm_towerst = towerst
                    , asm_taskdefs = let res = map generate tnodes in
                                     let (defs, mods) = unzip res in
                                     zip3 tnodes defs mods
                    , asm_system  = os_mkSysSchedule os tnodes
                    }

runTask :: Name -> Task () -> Tower TaskNode
runTask name t = do
  n <- freshname
  let uniquename = name ++ n
  (_, tnode) <- runStateT (emptyNodeSt uniquename emptyTaskSt) (unTask t)
  return $ tnode


-- Node Transformers----------------------------------------------------------

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

-- Task Getters/Setters --------------------------------------------------------

getTaskNode :: Task TaskNode
getTaskNode = Task get

getTaskSt :: Task TaskSt
getTaskSt = getTaskNode >>= \n -> return (nodest_impl n)

setTaskNode :: TaskNode -> Task ()
setTaskNode s = Task $ set s

setTaskSt :: TaskSt -> Task ()
setTaskSt s = getTaskNode >>= \n -> setTaskNode (n { nodest_impl = s })

getTaskName :: Task Name
getTaskName = do
  s <- getTaskNode
  return (nodest_name s)

taskStAddModuleDef :: (Schedule -> ModuleDef) -> Task ()
taskStAddModuleDef md = do
  s <- getTaskSt
  setTaskSt s { taskst_moddef = \sch -> taskst_moddef s sch >> md sch }

taskStAddChannelInit :: Def('[]:->()) -> Task ()
taskStAddChannelInit ci = do
  s <- getTaskSt
  setTaskSt $ s { taskst_channelinit = ci : (taskst_channelinit s) }

-- Tower Getters/Setters -------------------------------------------------------

getTowerSt :: Tower TowerSt
getTowerSt = Tower get

setTowerSt :: TowerSt -> Tower ()
setTowerSt s = Tower $ set s

