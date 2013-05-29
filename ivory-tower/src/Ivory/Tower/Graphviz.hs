
module Ivory.Tower.Graphviz
  ( graphvizDoc
  , graphvizToFile
  ) where

import Ivory.Tower.Types

import System.IO
import Text.PrettyPrint.Leijen

-- | Write a Tower 'Assembly' to a dot file
graphvizToFile :: FilePath -> Assembly -> IO ()
graphvizToFile f asm = withFile f WriteMode $ \h -> displayIO h rendered
  where
  w = 1000000 -- don't wrap lines - dot doesnt handle multiline strings
  rendered = renderPretty 1.0 w $ graphvizDoc asm

-- | Render a Tower 'Assembly' as a 'Text.PrettyPrint.Leijen.Doc'
graphvizDoc :: Assembly -> Doc
graphvizDoc a = vsep $
  [ text "digraph {"
  , indent 4 body
  , text "}"
  ]
  where
  towerst = asm_towerst a
  tasks = towerst_tasksts towerst
  body =  annotations
      <$> text "// task nodes"
      <$> vsep (map taskNode     tasks)
      <$> text "// dataport nodes"
      <$> vsep (map dataportNode (towerst_dataports towerst))
      <$> text "// channel nodes"
      <$> vsep (map channelNode  (towerst_channels towerst))
      <$> text ( "// emitter edges" ++ (show (concatMap taskst_emitters tasks)))
      <$> vsep (withEach emitterEdge  taskst_emitters    tasks)
      <$> text "// receiver edges"
      <$> vsep (withEach receiverEdge taskst_receivers   tasks)
      <$> text "// data reader edges"
      <$> vsep (withEach readerEdge   taskst_datareaders tasks)
      <$> text "// data writer edges"
      <$> vsep (withEach writerEdge   taskst_datawriters tasks)
      <$> text "// end"

  withEach f accessor ts = -- please excuse this, need coffee:
    concat $ map (\t -> map (\a' -> f t a') (accessor t)) ts

  annotations = text "graph [rankdir=LR];"
            <$> text "node [shape=record];"

-- Edge Naming Convention ------------------------------------------------------

chanName :: ChannelId -> String
chanName (ChannelId chid) = "channel" ++ (show chid)

dataportName :: DataportId -> String
dataportName (DataportId dpid) = "dataport" ++ (show dpid)

-- Task Node -------------------------------------------------------------------
taskNode :: TaskSt -> Doc
taskNode t =
  name <+> brackets (text "label=" <> dquotes contents) <> semi
  where
  name = text $ taskst_name t
  contents = hcat $ punctuate (text "|") fields
  fields = [ name <+> text ":: task" ]
        ++ prior ++ ssize
        ++ map periodic_field (taskst_periods t)
        ++ map emitter_field  (taskst_emitters t)
        ++ map receiver_field (taskst_receivers t)
        ++ map reader_field   (taskst_datareaders t)
        ++ map writer_field   (taskst_datawriters t)

  periodic_field p = text ("periodic @ " ++ (show p) ++ "ms")

  prior = case taskst_priority t of
    Just p -> [ text ("priority " ++ (show p)) ]
    Nothing -> []

  ssize = case taskst_stacksize t of
    Just s -> [ text ("stack size " ++ (show s)) ]
    Nothing -> []

  edge_field :: String -> String -> String -> Doc
  edge_field nm d1 d2 =
    angles (text nm) <+> text d1 <+> text d2

  emitter_field (Labeled chid descr) =
    edge_field (chanName chid)  descr "emitter"
  receiver_field (Labeled chid descr) =
    edge_field (chanName chid) descr "receiver"
  reader_field (Labeled dpid descr) =
    edge_field (dataportName  dpid) descr "reader"
  writer_field (Labeled dpid descr) =
    edge_field (dataportName dpid) descr "writer"

-- Dataport, Channel Nodes -----------------------------------------------------

dataportNode :: Labeled DataportId -> Doc
dataportNode (Labeled d tyname) =
  name <+> brackets (text "label=" <> dquotes contents) <> semi
  where
  contents = title <+> text ("|{<source>Source|<sink>Sink}")
  name = text $ dataportName d
  title = text "DataPort ::" <+> escapeQuotes (drop 2 tyname) -- drop Ty prefix

channelNode :: Labeled ChannelId -> Doc
channelNode (Labeled c tyname) =
  name <+> brackets (text "label=" <> dquotes contents) <> semi
  where
  contents = title <+> text ("|{<source>Source|<sink>Sink}")
  name = text $ chanName c
  title = text "Channel ::" <+> escapeQuotes (drop 2 tyname) -- drop Ty prefix

-- Edges -----------------------------------------------------------------------

emitterEdge :: TaskSt -> Labeled ChannelId -> Doc
emitterEdge task (Labeled chan _) = arrow tnode cnode
  where
  tnode = qual (taskst_name task) (chanName chan)
  cnode = qual (chanName chan) "source"

receiverEdge :: TaskSt -> Labeled ChannelId -> Doc
receiverEdge task (Labeled chan _) = arrow cnode tnode
  where
  cnode = qual (chanName chan) "sink"
  tnode = qual (taskst_name task) (chanName chan)

writerEdge :: TaskSt -> Labeled DataportId -> Doc
writerEdge task (Labeled dp _) = arrow tnode dnode
  where
  tnode = qual (taskst_name task) (dataportName dp)
  dnode = qual (dataportName dp) "source"

readerEdge :: TaskSt -> Labeled DataportId -> Doc
readerEdge task (Labeled dp _) = arrow dnode tnode
  where
  dnode = qual (dataportName dp) "sink"
  tnode = qual (taskst_name task) (dataportName dp)

-- Utility functions -----------------------------------------------------------

qual :: String -> String -> Doc
qual prefix name = text prefix <> colon <> text name

arrow :: Doc -> Doc -> Doc
arrow a b = a <+> text "->" <+> b <+> semi

escapeQuotes :: String -> Doc
escapeQuotes x = text $ aux x -- I know this is probably terrible (pch)
  where
  aux ('"':ss) = '\\' : '"' : (aux ss)
  aux  (s:ss)  = s : (aux ss)
  aux [] = []

