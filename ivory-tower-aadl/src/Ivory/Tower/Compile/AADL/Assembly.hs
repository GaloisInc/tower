
module Ivory.Tower.Compile.AADL.Assembly
  ( assemblyDoc
  ) where

import System.FilePath
import Debug.Trace

import Data.List (sortBy, groupBy, find)
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
  mapM_ taskDef   (asm_tasks asm)
  mapM_ signalDef (asm_sigs  asm)
  writeProcessDefinition (processDef name asm)
  where
  virtMod = package name $ do
    mapM_ depend mods

threadProp :: String -> String -> ThreadProperty
threadProp k v = ThreadProperty k (PropString v)

smaccmProp :: String -> String -> ThreadProperty
smaccmProp k v = threadProp ("SMACCM_SYS::" ++ k) v

taskDef :: AssembledNode TaskSt -> CompileM ()
taskDef asmtask = do
  features <- featuresDef asmtask (loopsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  nodest = an_nodest asmtask
  n = nodest_name nodest
  edges = nodest_edges nodest
  taskst = nodest_impl nodest
  loopsource = "tower_task_loop_" ++ n
  usersource = "tower_task_usercode_" ++ n
  props = [ threadProp "Source_Text" (usersource <.> "c") ]
        ++ initprop
        ++ periodprops
        ++ (map eventprop (taskst_evt_handlers taskst))
  initprop = case an_init asmtask of
    Just _ -> [ threadProp "Initialize_Source_Text" initdefname ]
    Nothing -> []

  -- XXX deprecate this once done debugging:
  eventprop act = UnprintableThreadProperty
    ((eimpl_str (act_evt act)) ++ (act_callname act))
  eimpl_str (ChannelEvent chid) = (eportname chid) ++ " event port handler "
  eimpl_str (PeriodEvent i) = "periodic (" ++ (show i) ++ "ms) event handler "

  periodprops = case uniq of
      [(a, bs)] -> mkPeriodProperty a bs
      [] -> [ threadProp "Dispatch" "Sporadic"]
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

  mkPeriodProperty interval callbacks =
    [ threadProp "Dispatch" "Hybrid"
    , ThreadProperty "Period" (PropUnit (fromIntegral interval) "ms")
    , ThreadProperty "Compute_Entrypoint_Source_Text" cbprop
    ]
    where
    cbprop = case callbacks of
      [c] -> PropString c
      cs -> PropList (map PropString cs)

  initdefname = "nodeInit_" ++ n -- magic: see Ivory.Tower.Node.nodeInit
  eportname chid = case find p (nodees_receivers edges) of
    Just e -> lbl_user e
    Nothing -> error msg
    where
    p e = unLabeled e == chid
    msg = "impossible: could not find chid " ++ (show (chan_id chid))
        ++ " in taskDef " ++ n

signalDef :: AssembledNode SignalSt -> CompileM ()
signalDef asmsig = do
  features <- featuresDef asmsig (commsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmsig)
  commsource = "tower_signal_comm_" ++ n 
  usersource = "tower_signal_usercode_" ++ n
  props = [ smaccmProp "Dispatch" "ISR"
          , threadProp "Source_Text" (usersource <.> "c")
          ] ++ isrprop ++ initprop
  isrprop = case signalst_cname (nodest_impl (an_nodest asmsig)) of
    Just signame -> [ smaccmProp "Signal_Name" signame ]
    Nothing -> []
  initprop = case an_init asmsig of
    Just _ -> [ threadProp "Initialize_Source_Text" initdefname ]
    Nothing -> []
  initdefname = "nodeInit_" ++ n -- magic: see Ivory.Tower.Node.nodeInit

featuresDef :: AssembledNode a -> FilePath -> CompileM [ThreadFeature]
featuresDef an headername = do
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
    return $ ThreadFeaturePort portname PortKindEvent Out chtype ps
    where
    ch = unLabeled lc
    portname = identifier (lbl_user lc)
    ps = channelprops (lbl_code lc) (chan_size ch)

  receiverDef :: Labeled ChannelId -> CompileM ThreadFeature
  receiverDef lc = do
    chtype <- channelTypename ch
    return $ ThreadFeaturePort portname PortKindEvent In chtype ps
    where
    ch = unLabeled lc
    portname = identifier (lbl_user lc)
    ps = channelprops (lbl_code lc) (chan_size ch)

  dataWriterDef :: Labeled DataportId -> CompileM ThreadFeature
  dataWriterDef (Labeled e n c) = do
    t <- dataportTypename e
    return $ ThreadFeaturePort (identifier n) PortKindData Out t (props c)

  dataReaderDef :: Labeled DataportId -> CompileM ThreadFeature
  dataReaderDef (Labeled e n c) = do
    t <- dataportTypename e
    return $ ThreadFeaturePort (identifier n) PortKindData In t (props c)

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

processDef :: String -> Assembly -> ProcessDef
processDef nodename asm = ProcessDef name components connections
  where
  name = identifier (nodename ++ "_process")
  components = [ pc t | t <- asm_tasks asm ]
            ++ [ pc s | s <- asm_sigs asm ]
  pc an = ProcessComponent (threadInstance n) n
    where n = identifier (nodest_name (an_nodest an))

  edges = [ nodest_edges (an_nodest at) | at <- asm_tasks asm ]
       ++ [ nodest_edges (an_nodest as) | as <- asm_sigs asm ]
  connections = chanConns edges ++ dataConns edges

chanConns :: [NodeEdges] -> [ProcessConnection]
chanConns edges =
  [ mkConn (nodees_name anode) aport (nodees_name bnode) bport
  | anode <- edges
  , aport <- nodees_emitters anode
  , bnode <- edges
  , bport <- nodees_receivers bnode
  , unLabeled aport == unLabeled bport
  ]

dataConns :: [NodeEdges] -> [ProcessConnection]
dataConns edges =
  [ mkConn (nodees_name anode) aport (nodees_name bnode) bport
  | anode <- edges
  , aport <- nodees_datawriters anode
  , bnode <- edges
  , bport <- nodees_datareaders bnode
  , unLabeled aport == unLabeled bport
  ]

mkConn :: String -> Labeled a -> String -> Labeled a -> ProcessConnection
mkConn n1 p1 n2 p2 = ProcessConnection (port n1 p1) (port n2 p2)
  where port n p = ProcessPort nn pp
          where nn = identifier (threadInstance n)
                pp = identifier (lbl_user p)

