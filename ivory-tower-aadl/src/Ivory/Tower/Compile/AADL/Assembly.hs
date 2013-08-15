
module Ivory.Tower.Compile.AADL.Assembly
  ( assemblyDoc
  ) where

import Ivory.Language
import Ivory.Tower.Types

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Identifier
import Ivory.Compile.AADL.Monad

assemblyDoc :: String -> [Module] -> Assembly -> Document
assemblyDoc name mods asm = runCompile mods virtMod $ do
  mapM_ (taskDef   asm) (asm_tasks asm)
  mapM_ (signalDef asm) (asm_sigs  asm)
  where
  virtMod = package name $ do
    mapM_ depend mods

taskDef :: Assembly -> AssembledNode TaskSt -> CompileM ()
taskDef asm asmtask = do
  features <- featuresDef asm asmtask
  writeThreadDefinition (t features)
  where
  t fs = ThreadDef name fs props
  name = nodest_name (an_nodest asmtask)
  props = []

signalDef :: Assembly -> AssembledNode SignalSt -> CompileM ()
signalDef asm asmsig = do
  features <- featuresDef asm asmsig
  writeThreadDefinition (t features)
  where
  t fs = ThreadDef name fs props
  name = nodest_name (an_nodest asmsig)
  props = []

featuresDef :: Assembly -> AssembledNode a -> CompileM [ThreadFeature]
featuresDef asm an = do
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
    t <- channelTypename asm e
    return $ ThreadFeaturePort (identifier n) PortKindEvent Out t props

  receiverDef :: Labeled ChannelId -> CompileM ThreadFeature
  receiverDef (Labeled e n) = do
    t <- channelTypename asm e
    return $ ThreadFeaturePort (identifier n) PortKindEvent In t props

  dataWriterDef :: Labeled DataportId -> CompileM ThreadFeature
  dataWriterDef (Labeled e n) = do
    t <- dataportTypename asm e
    return $ ThreadFeaturePort (identifier n) PortKindData Out t props

  dataReaderDef :: Labeled DataportId -> CompileM ThreadFeature
  dataReaderDef (Labeled e n) = do
    t <- dataportTypename asm e
    return $ ThreadFeaturePort (identifier n) PortKindData In t props

channelTypename :: Assembly -> ChannelId -> CompileM TypeName
channelTypename _ _ = return $ UnqualTypeName "FIXME_channelTypename"

dataportTypename :: Assembly -> DataportId -> CompileM TypeName
dataportTypename _ _ = return $ UnqualTypeName "FIXME_dataportTypename"

