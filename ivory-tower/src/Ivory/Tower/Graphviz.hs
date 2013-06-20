
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
  tasknodes = towerst_tasknodes towerst
  signodes  = towerst_signodes  towerst
  body =  annotations
      <$> text "// task nodes"
      <$> vsep (map taskNode (towerst_tasknodes towerst))
      <$> text "// signal nodes"
      <$> vsep (map sigNode (towerst_signodes towerst))
      <$> text "// dataport nodes"
      <$> vsep (map dataportNode (towerst_dataports towerst))
      <$> text "// channel nodes"
      <$> vsep (map channelNode  (towerst_channels towerst))
      <$> text "// emitter edges"
      <$> vsep (withEach emitterEdge  nodest_emitters    tasknodes)
      <$> vsep (withEach emitterEdge  nodest_emitters    signodes)
      <$> text "// receiver edges"
      <$> vsep (withEach receiverEdge nodest_receivers   tasknodes)
      <$> vsep (withEach receiverEdge nodest_receivers   signodes)
      <$> text "// data reader edges"
      <$> vsep (withEach readerEdge   nodest_datareaders tasknodes)
      <$> vsep (withEach readerEdge   nodest_datareaders signodes)
      <$> text "// data writer edges"
      <$> vsep (withEach writerEdge   nodest_datawriters tasknodes)
      <$> vsep (withEach writerEdge   nodest_datawriters signodes)
      <$> text "// end"

  withEach f accessor ts = -- please excuse this, need coffee:
    concat $ map (\t -> map (\a' -> f t a') (accessor t)) ts

  annotations = text "graph [rankdir=LR];"
            <$> text "node [shape=record];"

-- Edge Naming Convention ------------------------------------------------------

chanName :: ChannelId -> String
chanName (ChannelId chid _) = "channel" ++ (show chid)

dataportName :: DataportId -> String
dataportName (DataportId dpid) = "dataport" ++ (show dpid)

-- Task Node -------------------------------------------------------------------

taskNode :: TaskNode -> Doc
taskNode n = mkNode (text "task") (periods ++ prior ++ ssize) n
  where
  t = nodest_impl n

  periods = map periodic_field (taskst_periods t)
  periodic_field p = text ("periodic @ " ++ (show p) ++ "ms")

  prior = case taskst_priority t of
    Just p -> [ text ("priority " ++ (show p)) ]
    Nothing -> []

  ssize = case taskst_stacksize t of
    Just s -> [ text ("stack size " ++ (show s)) ]
    Nothing -> []


sigNode :: SigNode -> Doc
sigNode = mkNode (text "signal") []

mkNode :: Doc -> [Doc] -> NodeSt a  -> Doc
mkNode title auxfields n =
  name <+> brackets (text "label=" <> dquotes contents) <> semi
  where
  name = text $ nodest_name n
  contents = hcat $ punctuate (text "|") fields
  fields = [ name <+> text "::" <+> title ]
        ++ auxfields
        ++ map emitter_field  (nodest_emitters n)
        ++ map receiver_field (nodest_receivers n)
        ++ map reader_field   (nodest_datareaders n)
        ++ map writer_field   (nodest_datawriters n)

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
  contents = title <+> size <+> text ("|{<source>Source|<sink>Sink}")
  name = text $ chanName c
  size = text "|Size" <+> text (show (chan_size c))
  title = text "Channel ::" <+> escapeQuotes (drop 2 tyname) -- drop Ty prefix

-- Edges -----------------------------------------------------------------------

emitterEdge :: NodeSt a -> Labeled ChannelId -> Doc
emitterEdge node (Labeled chan _) = arrow tnode cnode
  where
  tnode = qual (nodest_name node) (chanName chan)
  cnode = qual (chanName chan) "source"

receiverEdge :: NodeSt a -> Labeled ChannelId -> Doc
receiverEdge node (Labeled chan _) = arrow cnode tnode
  where
  cnode = qual (chanName chan) "sink"
  tnode = qual (nodest_name node) (chanName chan)

writerEdge :: NodeSt a -> Labeled DataportId -> Doc
writerEdge node (Labeled dp _) = arrow tnode dnode
  where
  tnode = qual (nodest_name node) (dataportName dp)
  dnode = qual (dataportName dp) "source"

readerEdge :: NodeSt a -> Labeled DataportId -> Doc
readerEdge node (Labeled dp _) = arrow dnode tnode
  where
  dnode = qual (dataportName dp) "sink"
  tnode = qual (nodest_name node) (dataportName dp)

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

