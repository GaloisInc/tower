
module Ivory.Tower.Compile.AADL.Assembly
  ( assemblyDoc
  ) where

import System.FilePath
import Debug.Trace

import Data.List (sortBy, groupBy)
import Data.Maybe (catMaybes)

import Ivory.Language
import Ivory.Tower.Types

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Identifier
import Ivory.Compile.AADL.Monad
import Ivory.Compile.AADL.Gen (mkType)

assemblyDoc :: String -> [Module] -> Assembly -> Document
assemblyDoc name mods asm = runCompile mods virtMod $ do
  writeImport "SMACCM_SYS"
  let (prioritizedTasks, highest) = taskPriority (asm_tasks asm) 0
  mapM_ taskDef   prioritizedTasks
  mapM_ signalDef (zip (asm_sigs  asm) [highest..])
  writeProcessDefinition =<< (processDef name asm)
  where
  virtMod = package name $ do
    mapM_ depend mods

threadProp :: String -> String -> ThreadProperty
threadProp k v = ThreadProperty k (PropString v)

smaccmProp :: String -> String -> ThreadProperty
smaccmProp k v = threadProp ("SMACCM_SYS::" ++ k) v

taskDef :: (AssembledNode TaskSt, Int) -> CompileM ()
taskDef (asmtask, priority) = do
  features <- featuresDef asmtask (loopsource <.> "h") channelevts
  writeThreadDefinition (ThreadDef n features props)
  where
  nodest = an_nodest asmtask
  n = nodest_name nodest
  taskst = nodest_impl nodest
  loopsource = "tower_task_loop_" ++ n
  usersource = "tower_task_usercode_" ++ n
  props = [ ThreadProperty "Source_Text"
              (PropList [ PropString (usersource <.> "c") ])
          , ThreadProperty "Priority"
              (PropInteger (fromIntegral priority))
          ]
        ++ initprop
        ++ periodprops
  initprop = case an_init asmtask of
    Just _ -> [ threadProp "Initialize_Source_Text" initdefname ]
    Nothing -> []

  periodprops = case uniq of
      [(a, bs)] -> mkPeriodProperty a bs
      [] -> [ ThreadProperty "Dispatch_Protocol" (PropLiteral "Sporadic") ]
      _ -> trace warnmsg $ [UnprintableThreadProperty warnmsg ]
    where
    warnmsg = "Warning: multiple periodic rates in tower task named "
            ++ n ++ " cannot be rendered into an AADL property"
    uniq = map (\xs -> (fst (head xs), map snd xs))
         $ groupBy (\a b -> fst a == fst b)
         $ sortBy (\a b -> fst a `compare` fst b)
         $ catMaybes
         $ map aux (taskst_evt_handlers taskst)
    aux (Action (PeriodEvent i) cname _) = Just (i, cname)
    aux _ = Nothing

  channelevts = map (\xs -> (fst (head xs), map snd xs))
              $ groupBy (\a b -> fst a == fst b)
              $ sortBy (\a b -> fst a `compare` fst b)
              $ catMaybes
              $ map aux (taskst_evt_handlers taskst)
    where
    aux (Action (ChannelEvent chid) cname _) =
      Just (chid, escapeReserved cname)
    aux _ = Nothing

  mkPeriodProperty interval callbacks =
    [ ThreadProperty "Dispatch_Protocol" (PropLiteral "Hybrid")
    , ThreadProperty "Period" (PropUnit interval "ms")
    , ThreadProperty "Compute_Entrypoint_Source_Text" cbprop
    ]
    where
    cbprop = PropList (map PropString callbacks)

  initdefname = "nodeInit_" ++ n -- magic: see Ivory.Tower.Node.nodeInit

escapeReserved :: String -> String
escapeReserved "mode"  = "xMode"
escapeReserved "port"  = "xPort"
escapeReserved "group" = "xGroup"
escapeReserved a       = a

signalDef :: (AssembledNode SignalSt, Int) -> CompileM ()
signalDef (asmsig, priority) = do
  features <- featuresDef asmsig (commsource <.> "h") []
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmsig)
  commsource = "tower_signal_comm_" ++ n
  usersource = "tower_signal_usercode_" ++ n
  props = [ ThreadProperty "Dispatch_Protocol" (PropLiteral "Sporadic")
          , ThreadProperty "Source_Text"
              (PropList [ PropString (usersource <.> "c") ])
          , ThreadProperty "Priority"
              (PropInteger (fromIntegral priority))
          ] ++ isrprop ++ initprop
  isrprop = case signalst_cname (nodest_impl (an_nodest asmsig)) of
    Just signame -> [ smaccmProp "Signal_Name" signame ]
    Nothing -> []
  initprop = case an_init asmsig of
    Just _ -> [ threadProp "Initialize_Source_Text" initdefname ]
    Nothing -> []
  initdefname = "nodeInit_" ++ n -- magic: see Ivory.Tower.Node.nodeInit

featuresDef :: AssembledNode a -> FilePath -> [(ChannelId,[String])]
            -> CompileM [ThreadFeature]
featuresDef an headername channelevts = do
  ems <- mapM emitterDef    (nodees_emitters    edges)
  rxs <- mapM receiverDef   (nodees_receivers   edges)
  wrs <- mapM dataWriterDef (nodees_datawriters edges)
  res <- mapM dataReaderDef (nodees_datareaders edges)
  return $ concat [ems, rxs, wrs, res]

  where
  edges = nodest_edges (an_nodest an)
  props sourcetext =
    [ smaccmProp "CommPrim_Source_Header" headername
    , smaccmProp "CommPrim_Source_Text"   sourcetext
    ]
  channelprops sourcetext len = qp : props sourcetext
   where qp = ThreadProperty "Queue_Size" (PropInteger (fromIntegral len))

  emitterDef :: Labeled ChannelId -> CompileM ThreadFeature
  emitterDef lc = do
    chtype <- channelTypename ch
    return $ ThreadFeatureEventPort portname Out chtype ps
    where
    ch = unLabeled lc
    portname = identifier (lbl_user lc)
    ps = channelprops (lbl_code lc) (chan_size ch)

  receiverDef :: Labeled ChannelId -> CompileM ThreadFeature
  receiverDef lc = do
    chtype <- channelTypename ch
    return $ ThreadFeatureEventPort portname In chtype (ps ++ cs)
    where
    ch = unLabeled lc
    portname = identifier (lbl_user lc)
    ps = channelprops (lbl_code lc) (chan_size ch)
    cs = case lookup (unLabeled lc) channelevts of
      Nothing -> []
      Just cbs  -> tp (PropList (map PropString cbs))
    tp prop = [ThreadProperty "Compute_Entrypoint_Source_Text" prop]


  dataWriterDef :: Labeled DataportId -> CompileM ThreadFeature
  dataWriterDef (Labeled e n c) = do
    t <- dataportTypename e
    return $ ThreadFeatureDataPort (identifier n) t (writable:(props c))
    where
    writable = ThreadProperty "Access_Right" (PropLiteral "write_only")

  dataReaderDef :: Labeled DataportId -> CompileM ThreadFeature
  dataReaderDef (Labeled e n c) = do
    t <- dataportTypename e
    return $ ThreadFeatureDataPort (identifier n) t (readable:(props c))
    where
    readable = ThreadProperty "Access_Right" (PropLiteral "write_only")

channelTypename :: ChannelId -> CompileM TypeName
channelTypename chid = do
  t <- mkType (chan_ityp chid)
  return $ implNonBaseTypes t

dataportTypename :: DataportId -> CompileM TypeName
dataportTypename dpid = do
  t <- mkType (dp_ityp dpid)
  return $ implNonBaseTypes t

-- HACK: user declared datatypes always need .impl appended
implNonBaseTypes :: TypeName -> TypeName
implNonBaseTypes t = case t of
  QualTypeName "Base_Types" _ -> t
  DotTypeName _ _ -> t
  _ -> DotTypeName t "impl"

threadInstance :: String -> String
threadInstance threadtypename = threadtypename ++ "_inst"

processDef :: String -> Assembly -> CompileM ProcessDef
processDef nodename asm = do
  dpcomps <- mapM dataportComponent (towerst_dataports (asm_towerst asm))
  let components = tcomps ++ dpcomps
  return $ ProcessDef name components connections
  where
  name = identifier (nodename ++ "_process")
  tcomps = [ pc t | t <- asm_tasks asm ]
        ++ [ pc s | s <- asm_sigs asm ]
  pc an = ProcessThread (threadInstance n) n
    where n = identifier (nodest_name (an_nodest an))

  edges = [ nodest_edges (an_nodest at) | at <- asm_tasks asm ]
       ++ [ nodest_edges (an_nodest as) | as <- asm_sigs asm ]
  connections = chanConns edges ++ dataConns edges

dataportComponent :: DataportId -> CompileM ProcessComponent
dataportComponent dpid = do
  t <- mkType (dp_ityp dpid)
  return $ ProcessData n t
  where
  n = "dataport" ++ (show (dp_id dpid))

chanConns :: [NodeEdges] -> [ProcessConnection]
chanConns edges =
  [ mkConn (nodees_name anode) aport (nodees_name bnode) bport
  | anode <- edges
  , aport <- nodees_emitters anode
  , bnode <- edges
  , bport <- nodees_receivers bnode
  , unLabeled aport == unLabeled bport
  ]
  where
  mkConn :: String -> Labeled a -> String -> Labeled a -> ProcessConnection
  mkConn n1 p1 n2 p2 = EventConnection c1 c2
    where
    c1 = port (escapeReserved n1) p1
    c2 = port (escapeReserved n2) p2
    port n p = ProcessPort nn pp
        where nn = identifier (threadInstance n)
              pp = identifier (lbl_user p)

dataConns :: [NodeEdges] -> [ProcessConnection]
dataConns edges =
  [ mkConn (nodees_name node) port
  | node <- edges
  , port <- (nodees_datawriters node) ++ (nodees_datareaders node)
  ]
  where
  mkConn :: String -> Labeled DataportId -> ProcessConnection
  mkConn n p = DataConnection dpname port
    where
    dpname = "dataport" ++ (show (dp_id (unLabeled p)))
    port = ProcessPort nn pp
    nn = identifier (threadInstance (escapeReserved n))
    pp = identifier (escapeReserved (lbl_user p))



taskPriority :: [AssembledNode TaskSt]
             -> Int
             -> ([(AssembledNode TaskSt, Int)], Int)
taskPriority tasks basepriority = (zip sortedTasks [basepriority..], highest)
  where
  sortedTasks = sortBy cmp tasks
  task_pri    = taskst_priority . nodest_impl . an_nodest
  cmp a b     = compare (task_pri a) (task_pri b)
  highest     = basepriority + (length tasks)
