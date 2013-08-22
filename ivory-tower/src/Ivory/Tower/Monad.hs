{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monad where

import MonadLib

import Ivory.Language
import Ivory.Language.Area (ivoryArea)

import Ivory.Tower.Types

-- Monad Runners ---------------------------------------------------------------

runBase :: Base a -> OS -> a
runBase b os = fst (runM (unBase b) os 0)

runTower :: Tower p () -> Base Assembly
runTower twr = do
  (_, towerst) <- runStateT emptyTowerSt (unTower twr)
  os <- getOS
  let tnodes  = towerst_tasknodes towerst
      snodes  = towerst_signodes towerst
      genTask = os_assembleTask   os tnodes snodes
      genSig  = os_assembleSignal os tnodes snodes
  return $ Assembly { asm_towerst = towerst
                    , asm_tasks   = map genTask tnodes
                    , asm_sigs    = map genSig  snodes
                    , asm_system  = os_mkSysSchedule os tnodes snodes
                    }

runTask :: Name -> Task p () -> Tower p TaskNode
runTask name t = do
  n <- freshname
  let uniquename = name ++ n
  (_, tnode) <- runStateT (emptyNodeSt uniquename emptyTaskSt) (unNode t)
  return $ tnode

runSignal :: Name -> Signal p () -> Tower p SigNode
runSignal name s = do
  n <- freshname
  let uniquename = name ++ n
  (_, snode) <- runStateT (emptyNodeSt uniquename emptySignalSt) (unNode s)
  return $ snode

-- Node Transformers----------------------------------------------------------

nodeStAddReceiver :: ChannelId -> String -> String -> Node i p ()
nodeStAddReceiver r l1 l2 = do
  n <- getNodeEdges
  setNodeEdges $ n { nodees_receivers = (Labeled r l1 l2)
               : (nodees_receivers n)}

nodeStAddEmitter :: ChannelId -> String -> String -> Node i p ()
nodeStAddEmitter r l1 l2 = do
  n <- getNodeEdges
  setNodeEdges $ n { nodees_emitters = (Labeled r l1 l2)
               : (nodees_emitters n)}

nodeStAddDataReader :: DataportId -> String -> String -> Node i p ()
nodeStAddDataReader cc l1 l2 = do
  n <- getNodeEdges
  setNodeEdges $ n { nodees_datareaders = Labeled cc l1 l2
                                        : nodees_datareaders n
                   }

nodeStAddDataWriter :: DataportId -> String -> String -> Node i p ()
nodeStAddDataWriter cc l1 l2 = do
  n <- getNodeEdges
  setNodeEdges $ n { nodees_datawriters = Labeled cc l1 l2
                                        : nodees_datawriters n
                   }

nodeStAddCodegen :: Def ('[] :-> ()) -> ModuleDef -> Node i p ()
nodeStAddCodegen i m = do
  n <- getNode
  setNode $ n { nodest_codegen = Codegen i m : nodest_codegen n }

getNode :: Node i p (NodeSt i)
getNode = Node get

getNodeEdges :: Node i p NodeEdges
getNodeEdges = getNode >>= \n -> return (nodest_edges n)

setNode :: NodeSt i -> Node i p ()
setNode n = Node $ set n

setNodeEdges :: NodeEdges -> Node i p ()
setNodeEdges e = do
  n <- getNode
  setNode $ n { nodest_edges = e }

getNodeName:: Node i p Name
getNodeName = do
  n <- getNodeEdges
  return (nodees_name n)


-- Task Getters/Setters --------------------------------------------------------

getTaskSt :: Task p TaskSt
getTaskSt = getNode >>= \n -> return (nodest_impl n)

setTaskSt :: TaskSt -> Task p ()
setTaskSt s = getNode >>= \n -> setNode (n { nodest_impl = s })

taskStAddEventRxer :: Action -> Task p ()
taskStAddEventRxer a = do
  s <- getTaskSt
  setTaskSt s { taskst_evt_rxers = a : taskst_evt_rxers s }

taskStAddEventHandler :: Action -> Task p ()
taskStAddEventHandler a = do
  s <- getTaskSt
  setTaskSt s { taskst_evt_handlers = a : taskst_evt_handlers s }

taskStAddModuleDef :: (TaskSchedule -> ModuleDef) -> Task p ()
taskStAddModuleDef md = do
  s <- getTaskSt
  setTaskSt s { taskst_moddef = \sch -> taskst_moddef s sch >> md sch }

taskStAddModuleDefUser :: ModuleDef -> Task p ()
taskStAddModuleDefUser md = do
  s <- getTaskSt
  setTaskSt s { taskst_moddef_user = taskst_moddef_user s >> md }

-- Signal Getters/Setters --------------------------------------------------------

getSignalSt :: Signal p SignalSt
getSignalSt = getNode >>= \n -> return (nodest_impl n)

setSignalSt :: SignalSt -> Signal p ()
setSignalSt s = getNode >>= \n -> setNode (n { nodest_impl = s })

sigStAddModuleDef :: (SigSchedule -> ModuleDef) -> Signal p ()
sigStAddModuleDef md = do
  s <- getSignalSt
  setSignalSt s { signalst_moddef = \sch -> signalst_moddef s sch >> md sch }

sigStAddModuleDefUser :: ModuleDef -> Signal p ()
sigStAddModuleDefUser md = do
  s <- getSignalSt
  setSignalSt s { signalst_moddef_user = signalst_moddef_user s >> md }

-- Tower Getters/Setters -------------------------------------------------------

getTowerSt :: Tower p TowerSt
getTowerSt = Tower get

setTowerSt :: TowerSt -> Tower p ()
setTowerSt s = Tower $ set s

addTaskNode :: TaskNode -> Tower p ()
addTaskNode n = do
  s <- getTowerSt
  setTowerSt $ s { towerst_tasknodes = n : towerst_tasknodes s }

addSigNode :: SigNode -> Tower p ()
addSigNode n = do
  s <- getTowerSt
  setTowerSt $ s { towerst_signodes = n : towerst_signodes s }

mkChannel :: (IvoryArea a) => Integer -> Proxy a -> Tower p ChannelId
mkChannel size t = do
  n <- fresh
  let cid = ChannelId { chan_id   = toInteger n
                      , chan_size = size
                      , chan_ityp = ivoryArea t
                      }
  st <- getTowerSt
  setTowerSt $ st { towerst_channels = cid : towerst_channels st }
  return cid

mkDataport :: (IvoryArea a) => Proxy a -> Tower p DataportId
mkDataport t = do
  n <- fresh
  let dpid = DataportId { dp_id   = toInteger n
                        , dp_ityp = ivoryArea t }
  st <- getTowerSt
  setTowerSt $ st { towerst_dataports = dpid : towerst_dataports st }
  return dpid

addDataportCodegen :: Def('[]:->()) -> ModuleDef -> Tower p ()
addDataportCodegen initializer mdef = do
    s <- getTowerSt
    setTowerSt $ s { towerst_dataportgen = Codegen initializer mdef
                                         : towerst_dataportgen s }



