{-#LANGUAGE ScopedTypeVariables#-}

module Ivory.Tower.Petri.PetriLockCoarsening 
  (petriTowerLockCoarsening) where

import Data.List
import Ivory.Tower.AST.Handler
import Ivory.Tower.AST.Monitor
import Ivory.Tower.AST.Tower
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal

import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Time
import Ivory.Tower.Petri.Petri
import Ivory.Tower.Types.Opts
-- import Prelude


makeInitName :: String
makeInitName = "__distribute_init" 

initialPetri :: PetriNet
initialPetri = 
  ([nodeInit],[transInit],[edgeInit])
  where
    nodeInit  = PetriNode
      { node_name     = "__init0"
      , node_m0       = 1
      , node_color    = "grey63"
      , node_k        = 1
      , node_subgraph = "__init"
      , node_group    = ""
      , node_label    = "" 
      }

    transInit = PetriTransition
      { trans_name     = makeInitName
      , trans_color    = "grey63"
      , trans_subgraph = "__init"
      , trans_group    = ""
      }
    edgeInit = PetriEdge
      { edge_dep   = node_name nodeInit
      , edge_arr   = trans_name transInit
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }



petriTowerLockCoarsening :: Tower -> PetriNet
petriTowerLockCoarsening ast = 
  foldr petriUnion initialPetri $ 
    (map petriMonitor $ tower_monitors ast) ++ 
    (map petriSyncChan $ tower_syncchans ast) ++
    (map petriPeriod $ tower_periods ast) ++ 
    (map petriSignal $ tower_signals ast) 


makeMonitorName :: Monitor -> String
makeMonitorName = showUnique . monitor_name

makeSubgraphMonitorName :: Monitor -> String
makeSubgraphMonitorName mon = "monitor_" ++ makeMonitorName mon

makeLockName :: Monitor -> Int -> String
makeLockName mon ide = "lock_" ++ (show ide) ++ "_" ++ (makeMonitorName mon)

petriMonitor :: Monitor -> PetriNet
petriMonitor mon = 
  let monitorNet = (map (makeLockNode) (zip [1..] $ glob), [], []) in
  foldr petriUnion monitorNet $ map (petriHandler $ mon) $ monitor_handlers mon
  where 
    (Just (LockCoarsening (OptMonitor glob))) = getOpt (LockCoarsening OptVoid) $ monitor_transformers mon
    
    makeLockNode :: (Int,[String]) -> PetriNode
    makeLockNode (ide, list) = PetriNode
      { node_name     = makeLockName mon ide
      , node_m0       = 1
      , node_color    = "deeppink"
      , node_k        = 1
      , node_subgraph = makeSubgraphMonitorName mon
      , node_group    = ""
      , node_label    = concat $ intersperse ", " $ list 
      }

makeHandlerName :: Handler -> String
makeHandlerName h= "han_" ++ (showUnique $ handler_name h)

makeGroupHandlerName :: Handler -> String
makeGroupHandlerName h = "g" ++ (makeHandlerName h)

makeListener :: Chan -> String

makeListener (ChanSync schan) = "distribute_" ++ (makeSyncChanName schan)
makeListener (ChanSignal sig) = "rearm_" ++ (makeSignalName sig)
makeListener (ChanPeriod per) = "loop_" ++ (makePeriodName per)
makeListener (ChanInit _) = makeInitName


petriHandler :: Monitor -> Handler -> PetriNet
petriHandler mon h =
  ([handlerReady, handlerComputing]++(addFineLocks $ length locksToTake),[handlerLock, handlerRelease]++(addFineLocksT $ length locksToTake),
    [subscribe, takeLockHan, enterComputation $ length locksToTake, finishComputation] ++ emittersEdges ++ 
    (addFineLocksE $ length locksToTake) ++ (map takeLockMon $ zip [1..] locksToTake) ++ (map releaseLock locksToTake))
  where
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ monitor_transformers mon
    (Just (LockCoarsening (OptHandler globhan))) = getOpt (LockCoarsening OptVoid) $ handler_transformers h
    locksToTake :: [Int]
    locksToTake = sort $ map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ globmon) $ globhan

    handlerReady      = PetriNode
      { node_name     = makeHandlerName h
      , node_m0       = 0
      , node_color    = "purple"
      , node_k        = 1
      , node_subgraph = makeSubgraphMonitorName mon
      , node_group    = makeGroupHandlerName h
      , node_label    = concat $ intersperse ", " $ globhan
      }
    handlerComputing = PetriNode
      { node_name     = "comp_" ++ (makeHandlerName h)
      , node_m0       = 0
      , node_color    = "blue"
      , node_k        = 1
      , node_subgraph = makeSubgraphMonitorName mon
      , node_group    = makeGroupHandlerName h
      , node_label    = "" 
      }
    handlerLock = PetriTransition
      { trans_name     = "lock_" ++ (makeHandlerName h)
      , trans_color    = "plum1"
      , trans_subgraph = makeSubgraphMonitorName mon
      , trans_group    = makeGroupHandlerName h
      }
    handlerRelease = PetriTransition
      { trans_name     = "release_" ++ (makeHandlerName h)
      , trans_color    = "plum1"
      , trans_subgraph = makeSubgraphMonitorName mon
      , trans_group    = makeGroupHandlerName h
      }
    subscribe = PetriEdge
      { edge_dep   = makeListener $ handler_chan h
      , edge_arr   = node_name handlerReady
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }
    takeLockHan = PetriEdge
      { edge_dep   = node_name handlerReady
      , edge_arr   = trans_name handlerLock
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }
    finishComputation = PetriEdge
      { edge_dep   = node_name handlerComputing
      , edge_arr   = trans_name handlerRelease
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }

    -- we create an edge from lock_name if <= 1 lock to take, and from lock_lastid_name if > 1.
    enterComputation 0 = PetriEdge
      { edge_dep   = trans_name handlerLock
      , edge_arr   = node_name handlerComputing
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }
    enterComputation 1 = enterComputation 0
    enterComputation n = PetriEdge
      { edge_dep   = "lock" ++ (show n) ++ "_" ++ (makeHandlerName h)
      , edge_arr   = node_name handlerComputing
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }

    addFineLocks n = case n of
      0 -> []
      1 -> []
      _ -> PetriNode
        { node_name     = "locked" ++ (show $ n-1) ++ "_" ++ (makeHandlerName h)
        , node_m0       = 0
        , node_color    = "blue"
        , node_k        = 1
        , node_subgraph = makeSubgraphMonitorName mon
        , node_group    = makeGroupHandlerName h
        , node_label    = "" 
        } : (addFineLocks $ n-1)

    addFineLocksT n = case n of
      0 -> []
      1 -> []
      _ -> PetriTransition
        { trans_name     = "lock" ++ (show n) ++ "_" ++ (makeHandlerName h)
        , trans_color    = "plum1"
        , trans_subgraph = makeSubgraphMonitorName mon
        , trans_group    = makeGroupHandlerName h
        } : (addFineLocksT $ n-1)

    addFineLocksE n = case n of
      0 -> []
      1 -> []
      2 -> [ PetriEdge
        { edge_dep   = "lock_" ++ (makeHandlerName h)
        , edge_arr   = "locked1_" ++ (makeHandlerName h)
        , edge_w     = 1
        , edge_color = "black"
        , edge_label = ""
        } 
        , PetriEdge
        { edge_dep   = "locked1_" ++ (makeHandlerName h)
        , edge_arr   = "lock2_" ++ (makeHandlerName h)
        , edge_w     = 1
        , edge_color = "black"
        , edge_label = ""
        }]
      _ -> [ PetriEdge
        { edge_dep   = "lock" ++ (show $ n-1) ++ "_" ++ (makeHandlerName h)
        , edge_arr   = "locked" ++ (show $ n-1) ++ "_" ++ (makeHandlerName h)
        , edge_w     = 1
        , edge_color = "black"
        , edge_label = ""
        } 
        , PetriEdge
        { edge_dep   = "locked" ++ (show $ n-1) ++ "_" ++ (makeHandlerName h)
        , edge_arr   = "lock" ++ (show $ n) ++ "_" ++ (makeHandlerName h)
        , edge_w     = 1
        , edge_color = "black"
        , edge_label = ""
        }] ++ (addFineLocksE $ n-1)

    takeLockMon :: (Int, Int) -> PetriEdge
    takeLockMon (idx,ide) = PetriEdge
      { edge_dep   = makeLockName mon ide
      , edge_arr   = case idx of 
        1 -> "lock_" ++ (makeHandlerName h)
        _ -> "lock" ++ (show $ idx) ++ "_" ++ (makeHandlerName h)
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = "\", constraint=\"false" -- add , constraint=false using the same technique as SQL injections
      }
    releaseLock :: Int -> PetriEdge
    releaseLock ide = PetriEdge
      { edge_dep   = trans_name handlerRelease
      , edge_arr   = makeLockName mon ide
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }
    emittersEdges = map (makeEdgeEmitter $ trans_name handlerRelease) $ handler_emitters h

makeEdgeEmitter :: String -> Emitter -> PetriEdge
makeEdgeEmitter origin emit = PetriEdge
      { edge_dep   = origin
      , edge_arr   = makeSyncChanName $ emitter_chan emit
      , edge_w     = fromInteger $ emitter_bound emit
      , edge_color = "black"
      , edge_label = ""
      }

queueSize :: Int
queueSize = 1000

makeSyncChanName :: SyncChan -> String
makeSyncChanName chan = 
  "chan_" ++ (show $ sync_chan_label chan)

makeSubgraphSyncChanName :: SyncChan -> String
makeSubgraphSyncChanName chan = 
  "channel_" ++ (show $ sync_chan_label chan)

petriSyncChan :: SyncChan -> PetriNet
petriSyncChan chan =
  ([nodeChan],[transDistribute],[edgeChanDist])
  where
    nodeChan  = PetriNode
      { node_name     = makeSyncChanName chan
      , node_m0       = 0
      , node_color    = "cyan"
      , node_k        = queueSize
      , node_subgraph = makeSubgraphSyncChanName chan
      , node_group    = ""
      , node_label    = "" 
      }

    transDistribute = PetriTransition
      { trans_name     = "distribute_" ++ (makeSyncChanName chan)
      , trans_color    = "green"
      , trans_subgraph = makeSubgraphSyncChanName chan
      , trans_group    = ""
      }
    edgeChanDist = PetriEdge
      { edge_dep   = node_name nodeChan
      , edge_arr   = trans_name transDistribute
      , edge_w     = 1
      , edge_color = "black"
      , edge_label = ""
      }



makePeriodName :: Period -> String
makePeriodName per = 
  "per_" ++ (show . toMicroseconds $ period_dt per)

makeSubgraphPeriodName :: Period -> String
makeSubgraphPeriodName per = 
  "period_" ++ (show . toMicroseconds $ period_dt per) ++ "_μs"

petriPeriod :: Period -> PetriNet
petriPeriod per =
  ([nodePer],[transClk],[edgePerClk, edgeClkPer]) 
  where
    nodePer  = PetriNode
      { node_name     = makePeriodName per
      , node_m0       = 1
      , node_color    = "orangered"
      , node_k        = 1
      , node_subgraph = makeSubgraphPeriodName per
      , node_group    = ""
      , node_label    = "" 
      }
    transClk = PetriTransition
      { trans_name     = "loop_" ++ (makePeriodName per)
      , trans_color    = "orangered"
      , trans_subgraph = makeSubgraphPeriodName per
      , trans_group    = ""
      }
    edgePerClk = PetriEdge
      { edge_dep   = node_name nodePer
      , edge_arr   = trans_name transClk
      , edge_w     = 1
      , edge_color = "orangered"
      , edge_label = ""
      }
    edgeClkPer = PetriEdge
      { edge_dep   = trans_name transClk
      , edge_arr   = node_name nodePer
      , edge_w     = 1
      , edge_color = "orangered"
      , edge_label = ""
      }


makeSignalName :: Signal -> String
makeSignalName sig = 
  "sig_" ++ (signal_name sig)

makeSubgraphSignalName :: Signal -> String
makeSubgraphSignalName sig = 
  "signal_" ++ (signal_name sig)

petriSignal :: Signal -> PetriNet
petriSignal sig =
  ([nodeSig],[transRearm],[edgeSigRearm, edgeRearmSig]) 
  where
    nodeSig  = PetriNode
      { node_name     = makeSignalName sig
      , node_m0       = 1
      , node_color    = "palegreen"
      , node_k        = 1
      , node_subgraph = makeSubgraphSignalName sig
      , node_group    = ""
      , node_label    = "" 
      }
    transRearm = PetriTransition
      { trans_name     = "rearm_" ++ (makeSignalName sig)
      , trans_color    = "palegreen"
      , trans_subgraph = makeSubgraphSignalName sig
      , trans_group    = ""
      }
    edgeSigRearm = PetriEdge
      { edge_dep   = node_name nodeSig
      , edge_arr   = trans_name transRearm
      , edge_w     = 1
      , edge_color = "palegreen"
      , edge_label = ""
      }
    edgeRearmSig = PetriEdge
      { edge_dep   = trans_name transRearm
      , edge_arr   = node_name nodeSig
      , edge_w     = 1
      , edge_color = "palegreen"
      , edge_label = ""
      }