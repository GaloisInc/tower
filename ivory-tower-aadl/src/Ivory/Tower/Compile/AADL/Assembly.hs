
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
  mapM_ taskDef   (asm_tasks asm)
  mapM_ signalDef (asm_sigs  asm)
  where
  virtMod = package name $ do
    mapM_ depend mods

taskDef :: AssembledNode TaskSt -> CompileM ()
taskDef asmtask = do
  features <- featuresDef asmtask (loopsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmtask)
  loopsource = "tower_task_loop_" ++ n
  usersource = "tower_task_usercode_" ++ n
  props = [ ThreadProperty "Source_Text" (dquotes (usersource <.> "c"))
          ]

signalDef :: AssembledNode SignalSt -> CompileM ()
signalDef asmsig = do
  features <- featuresDef asmsig (commsource <.> "h")
  writeThreadDefinition (ThreadDef n features props)
  where
  n = nodest_name (an_nodest asmsig)
  commsource = "tower_signal_comm_" ++ n 
  usersource = "tower_signal_usercode_" ++ n
  props = [ ThreadProperty "Source_Text" (dquotes (usersource <.> "c"))
          ]

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
    [ ("Compute_Entrypoint_Source_Header", dquotes headername)
    , ("Compute_Entrypoint_Source_Text", dquotes sourcetext)
    ]
  channelprops sourcetext len = ("Queue_Size", show len) : props sourcetext

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
  _ -> DotTypeName t "impl"

dquotes :: String -> String
dquotes s = "\"" ++ s ++ "\""

