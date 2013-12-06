
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
import Ivory.Compile.AADL.Gen (mkType, typeImpl)

assemblyDoc :: String -> [Module] -> Assembly -> TypeCtxM (Document, [Warning])
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

taskDef :: (AssembledNode TaskSt, Int) -> CompileM Unique ()
taskDef (asmtask, priority) = do
  threadname <- introduceUnique (nodees_name (nodest_edges nodest))
  features <- featuresDef asmtask (loopsource <.> "h") channelevts
  writeThreadDefinition (ThreadDef threadname features props)
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
          , ThreadProperty "Source_Stack_Size"
              (PropUnit (fromIntegral (taskst_stacksize taskst)) "bytes")
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
    , ThreadProperty "SMACCM_SYS::Compute_Entrypoint_Source_Text" cbprop
    ]
    where
    cbprop = PropList (map PropString callbacks)

  initdefname = "nodeInit_" ++ n -- magic: see Ivory.Tower.Node.nodeInit

signalDef :: (AssembledNode SignalSt, Int) -> CompileM Unique ()
signalDef (asmsig, priority) = do
  signame <- introduceUnique (nodees_name (nodest_edges (an_nodest asmsig)))
  features <- featuresDef asmsig (commsource <.> "h") []
  writeThreadDefinition (ThreadDef signame features props)
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

introduceUnique :: Unique -> CompileM Unique String
introduceUnique u = do
  m <- getIdentifierMap
  case elem up (map snd m) of
    False -> do
      setIdentifierMap ((u,up):m)
      return up
    True -> do
      uniquenessWarning warning
      notUnique m (showUnique u)
  where
  up = unique_userprovided u
  warning = "User provided identifier " ++ up
         ++ " is not unique in tower-aadl Assembly."
  notUnique m r = case elem r (map snd m) of
    True -> do
      uniquenessWarning (r ++ " insufficiently unique, expanding")
      notUnique m (r ++ "_1")
    False -> do
      setIdentifierMap ((u,r):m)
      return r

uniqueIdentifier :: Unique -> String -> CompileM Unique String
uniqueIdentifier u ctx = do
  m <- getIdentifierMap
  -- If this lookup fails, its because the CompileM code that should have used
  -- introduceUnique to create names was not implemented correctly.
  case lookup u m of
    Just n -> return n
    Nothing -> error ("Failed to find unique identifier " ++ (showUnique u)
                      ++ " in " ++ ctx ++ " context.\nName Map Dump\n" ++ (show m))

featuresDef :: AssembledNode a -> FilePath -> [(ChannelId,[String])]
            -> CompileM Unique [ThreadFeature]
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

  emitterDef :: (ChannelId, Unique, SymbolName) -> CompileM Unique ThreadFeature
  emitterDef (chid, uname, symname) = do
    chtype <- channelTypename chid
    portname <- introduceUnique uname
    return $ ThreadFeatureEventPort portname Out chtype ps
    where
    ps = channelprops symname (chan_size chid)

  receiverDef :: (ChannelId, Unique, SymbolName) -> CompileM Unique ThreadFeature
  receiverDef (chid, uname, symname) = do
    chtype <- channelTypename chid
    portname <- introduceUnique uname
    return $ ThreadFeatureEventPort portname In chtype (ps ++ cs)
    where
    ps = channelprops symname (chan_size chid)
    cs = case lookup chid channelevts of
      Nothing -> []
      Just cbs  -> tp (PropList (map PropString cbs))
    tp prop = [ThreadProperty "SMACCM_SYS::Compute_Entrypoint_Source_Text" prop]


  dataWriterDef :: (DataportId, Unique, SymbolName) -> CompileM Unique ThreadFeature
  dataWriterDef (dpid, uniq, symname) = do
    t <- dataportTypename dpid
    ident <- introduceUnique uniq
    return $ ThreadFeatureDataPort ident t (writable:(props symname))
    where
    writable = ThreadProperty "Access_Right" (PropLiteral "write_only")

  dataReaderDef :: (DataportId, Unique, SymbolName) -> CompileM Unique ThreadFeature
  dataReaderDef (dpid, uniq, symname) = do
    t <- dataportTypename dpid
    ident <- introduceUnique uniq
    return $ ThreadFeatureDataPort ident t (readable:(props symname))
    where
    readable = ThreadProperty "Access_Right" (PropLiteral "read_only")

channelTypename :: ChannelId -> CompileM Unique TypeName
channelTypename chid = do
  t <- mkType (chan_ityp chid)
  return $ implNonBaseTypes t

dataportTypename :: DataportId -> CompileM Unique TypeName
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

processDef :: String -> Assembly -> CompileM Unique ProcessDef
processDef nodename asm = do
  dpcomps <- mapM dataportComponent (towerst_dataports (asm_towerst asm))
  let components = tcomps ++ dpcomps
  chanconns <- chanConns edges
  dataconns <- dataConns edges
  return $ ProcessDef name components (chanconns ++ dataconns)
  where
  name = identifier (nodename ++ "_process")
  tcomps = [ pc t | t <- asm_tasks asm ]
        ++ [ pc s | s <- asm_sigs asm ]
  pc an = ProcessThread (threadInstance n) n
    where n = identifier (nodest_name (an_nodest an))
  edges = [ nodest_edges (an_nodest at) | at <- asm_tasks asm ]
       ++ [ nodest_edges (an_nodest as) | as <- asm_sigs asm ]

dataportComponent :: DataportId -> CompileM Unique ProcessComponent
dataportComponent dpid = do
  t <- mkType (dp_ityp dpid)
  return $ ProcessData n (typeImpl t)
  where
  n = "dataport" ++ (show (dp_id dpid))

chanConns :: [NodeEdges] -> CompileM Unique [ProcessConnection]
chanConns edges = sequence
  [ mkConn (nodees_name anode) aport (nodees_name bnode) bport
  | anode <- edges
  , aport <- nodees_emitters anode
  , bnode <- edges
  , bport <- nodees_receivers bnode
  , ci aport == ci bport
  ]
  where
  ci (a, _, _) = a
  mkConn :: Unique -> (ChannelId, Unique, SymbolName)
         -> Unique -> (ChannelId, Unique, SymbolName)
         -> CompileM Unique ProcessConnection
  mkConn n1 (_,p1,_) n2 (_,p2,_) = do
    c1 <- processPort n1 p1
    c2 <- processPort n2 p2
    return $ EventConnection c1 c2
    where
    processPort n p = do
      nn <- uniqueIdentifier n "chanConns mkConn node name"
      pp <- uniqueIdentifier p "chanConns mkConn node port"
      return $ ProcessPort (threadInstance nn) pp

dataConns :: [NodeEdges] -> CompileM Unique [ProcessConnection]
dataConns edges = sequence
  [ mkConn (nodees_name node) port
  | node <- edges
  , port <- (nodees_datawriters node) ++ (nodees_datareaders node)
  ]
  where
  mkConn :: Unique
         -> (DataportId, Unique, SymbolName)
         -> CompileM Unique ProcessConnection
  mkConn nodename (dpid,portname,_) = do
    nn <- uniqueIdentifier nodename "dataConns node name"
    pp <- uniqueIdentifier portname "dataConns port name"
    let port = ProcessPort (threadInstance nn) pp
    return $ DataConnection dpname port
    where
    dpname = "dataport" ++ (show (dp_id dpid))


taskPriority :: [AssembledNode TaskSt]
             -> Int
             -> ([(AssembledNode TaskSt, Int)], Int)
taskPriority tasks basepriority = (zip sortedTasks [basepriority..], highest)
  where
  sortedTasks = sortBy cmp tasks
  task_pri    = taskst_priority . nodest_impl . an_nodest
  cmp a b     = compare (task_pri a) (task_pri b)
  highest     = basepriority + (length tasks)
