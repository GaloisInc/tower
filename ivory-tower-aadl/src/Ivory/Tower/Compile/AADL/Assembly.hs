
module Ivory.Tower.Compile.AADL.Assembly
  ( assemblyDoc
  ) where

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
  features <- featuresDef asmtask
  writeThreadDefinition (t features)
  where
  t fs = ThreadDef name fs props
  name = nodest_name (an_nodest asmtask)
  props = []

signalDef :: AssembledNode SignalSt -> CompileM ()
signalDef asmsig = do
  features <- featuresDef asmsig
  writeThreadDefinition (t features)
  where
  t fs = ThreadDef name fs props
  name = nodest_name (an_nodest asmsig)
  props = []

featuresDef :: AssembledNode a -> CompileM [ThreadFeature]
featuresDef an = do
  ems <- mapM emitterDef    (nodees_emitters    edges)
  rxs <- mapM receiverDef   (nodees_receivers   edges)
  wrs <- mapM dataWriterDef (nodees_datawriters edges)
  res <- mapM dataReaderDef (nodees_datareaders edges)
  return (ems ++ rxs ++ wrs ++ res)

  where
  edges = nodest_edges (an_nodest an)
  props = [("thread_feature_propertes","punted_on")] -- XXX

  emitterDef :: Labeled ChannelId -> CompileM ThreadFeature
  emitterDef (Labeled e n) = do
    t <- channelTypename e
    return $ ThreadFeaturePort (identifier n) PortKindEvent Out t props

  receiverDef :: Labeled ChannelId -> CompileM ThreadFeature
  receiverDef (Labeled e n) = do
    t <- channelTypename e
    return $ ThreadFeaturePort (identifier n) PortKindEvent In t props

  dataWriterDef :: Labeled DataportId -> CompileM ThreadFeature
  dataWriterDef (Labeled e n) = do
    t <- dataportTypename e
    return $ ThreadFeaturePort (identifier n) PortKindData Out t props

  dataReaderDef :: Labeled DataportId -> CompileM ThreadFeature
  dataReaderDef (Labeled e n) = do
    t <- dataportTypename e
    return $ ThreadFeaturePort (identifier n) PortKindData In t props

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

