
module Ivory.Tower.Compile.AADL.Assembly
  ( assemblyDoc
  ) where

import System.FilePath

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

smaccmProp :: String -> String -> ThreadProperty
smaccmProp k v = ThreadProperty  ("SMACCM_SYS::" ++ k) v

taskDef :: AssembledNode TaskSt -> CompileM ()
taskDef asmtask = do
  features <- featuresDef asmtask (loopsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmtask)
  loopsource = "tower_task_loop_" ++ n
  usersource = "tower_task_usercode_" ++ n
  props = [ smaccmProp "Dispatch" (dquotes "EventLoop")
          , ThreadProperty "Source_Text" (dquotes (usersource <.> "c"))
          ] ++ initprop
  initprop = case taskst_taskinit (nodest_impl (an_nodest asmtask)) of
    Just _ -> [ ThreadProperty "Initialize_Source_Text" (dquotes initdefname) ]
    Nothing -> []
  initdefname = "taskInit_" ++ n -- magic: see Ivory.Tower.Task.taskInit

signalDef :: AssembledNode SignalSt -> CompileM ()
signalDef asmsig = do
  features <- featuresDef asmsig (commsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmsig)
  commsource = "tower_signal_comm_" ++ n 
  usersource = "tower_signal_usercode_" ++ n
  props = [ smaccmProp "Dispatch" (dquotes "ISR")
          , ThreadProperty "Source_Text" (dquotes (usersource <.> "c"))
          ] ++ isrprop
  isrprop = case signalst_cname (nodest_impl (an_nodest asmsig)) of
    Just signame -> [ smaccmProp "Signal_Name" (dquotes signame) ]
    Nothing -> []

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
    [ smaccmProp "CommPrim_Source_Header" (dquotes headername)
    , smaccmProp "CommPrim_Source_Text"   (dquotes sourcetext)
    ]
  channelprops sourcetext len = qp : props sourcetext
   where qp = ThreadProperty "Queue_Size" (show len)

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

dquotes :: String -> String
dquotes s = "\"" ++ s ++ "\""

processDef :: String -> Assembly -> ProcessDef
processDef n asm = ProcessDef name components connections
  where
  name = identifier (n ++ "_process")
  components = [ pc t | t <- asm_tasks asm ]
            ++ [ pc s | s <- asm_sigs asm ]
  pc an = ProcessComponent (n ++ "_inst") n
    where n = identifier (nodest_name (an_nodest an))

  channels = towerst_channels  (asm_towerst asm)
  dataports = towerst_dataports (asm_towerst asm)
  edges = [ nodest_edges (an_nodest at) | at <- asm_tasks asm ]
       ++ [ nodest_edges (an_nodest as) | as <- asm_sigs asm ]

  connections = concatMap (chanConns edges) channels
             ++ concatMap (dataConns edges) dataports

chanConns :: [NodeEdges] -> ChannelId -> [ProcessConnection]
chanConns es chid = [] -- XXX search all node edges for emitters matching chid,
                       -- match those to all node edges with receivers matching chid

dataConns :: [NodeEdges] -> DataportId -> [ProcessConnection]
dataConns es dpid = [] -- XXX same basic algorithm

