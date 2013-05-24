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
runTower t = do
  (_, towerst) <- runStateT emptyTowerSt (unTower t)
  os <- getOS 
  let channels = towerst_channels towerst
      tasks    = towerst_tasksts  towerst

      generate :: TaskSt -> Def('[]:->())
      generate aTask = scheduleTaskBody sch
        where sch = os_mkSchedule os channels tasks aTask
              scheduleTaskBody = case taskst_taskbody aTask of
                Just b -> b
                Nothing -> error ("runTower input Error: Missing task body in " 
                                  ++ (taskst_name aTask))

  return $ Assembly { asm_towerst = towerst
                    , asm_taskdefs = map generate tasks
                    }


runTask :: Name -> Task () -> Tower TaskSt
runTask name t = do
  n <- freshname
  let uniquename = name ++ n
  (_, taskSt) <- runStateT (emptyTaskSt uniquename) (unTask t)
  return $ taskSt


-- Task Getters/Setters --------------------------------------------------------

getTaskSt :: Task TaskSt
getTaskSt  = Task get

setTaskSt :: TaskSt -> Task ()
setTaskSt s = Task $ set s

getTaskName :: Task Name
getTaskName = do
  s <- getTaskSt
  return (taskst_name s) 

taskStAddReceiver :: ChannelId -> String -> Task ()
taskStAddReceiver r lbl = do
  s <- getTaskSt
  setTaskSt $ s { taskst_receivers = (Labeled r lbl) : (taskst_receivers s)}

taskStAddEmitter :: ChannelId -> String -> Task ()
taskStAddEmitter r lbl = do
  s <- getTaskSt
  setTaskSt $ s { taskst_emitters = (Labeled r lbl) : (taskst_emitters s)}

taskStAddDataReader :: CompiledChannel -> String -> Task ()
taskStAddDataReader cc lbl = do
  s <- getTaskSt
  setTaskSt $ s { taskst_datareaders = (Labeled cc lbl) : (taskst_datareaders s)}

taskStAddDataWriter :: CompiledChannel -> String -> Task ()
taskStAddDataWriter cc lbl = do
  s <- getTaskSt
  setTaskSt $ s { taskst_datawriters = (Labeled cc lbl) : (taskst_datawriters s)}

taskStAddModuleDef :: ModuleDef -> Task ()
taskStAddModuleDef md = do
  s <- getTaskSt
  setTaskSt $ s { taskst_moddef = (taskst_moddef s) >> md }

taskStAddChannelInit :: Def('[]:->()) -> Task ()
taskStAddChannelInit ci = do
  s <- getTaskSt
  setTaskSt $ s { taskst_channelinit = ci : (taskst_channelinit s) }

-- Tower Getters/Setters -------------------------------------------------------

getTowerSt :: Tower TowerSt
getTowerSt = Tower get

setTowerSt :: TowerSt -> Tower ()
setTowerSt s = Tower $ set s

