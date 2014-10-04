
module Ivory.Tower.AST.Graph where

import Data.Graph
import Data.List (groupBy)
import Ivory.Tower.AST.Thread
import Ivory.Tower.AST.Handler
import Ivory.Tower.AST.Monitor
import Ivory.Tower.AST.Tower
import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter

import Ivory.Tower.Types.Unique

import Text.PrettyPrint.Mainland

handlerGraph :: Tower
             -> (Graph, (Vertex -> ((Monitor, Handler), Handler, [Handler])), Handler -> Maybe Vertex)
handlerGraph t = graphFromEdges $ do
  m <- tower_monitors t
  h <- monitor_handlers m
  return ((m,h), h, handlerOutboundHandlers t h)

graphviz :: (Graph, (Vertex -> ((Monitor, Handler), Handler, [Handler])), Handler -> Maybe Vertex)
                    -> String
graphviz (g, unvertex, _) = pretty 80 $ stack $
  [ text "digraph Tower {"
  , indent 4 $ stack $ map ppSubgraph monitors
  , empty
  , indent 4 (stack (map ppEdge (edges g)))
  , text "}"
  ]
  where
  monitors = groupBy (\((a,_),_,_) ((b,_),_,_) -> a == b)
                     (map unvertex (vertices g))
  ppSubgraph ms@(((m,_),_,_):_) = stack
    [ text "subgraph " <+> text "cluster_" <> mname <+> text "{"
    , indent 4 $ text "color = blue" <> semi
    , indent 4 $ text "node [style=filled]" <> semi
    , indent 4 $ stack $ map (\h -> ppHandlerNode h <> semi) ms
    , indent 4 $ text "label =" <+> dquotes (text "monitor" <+> mname) <> semi
    , text "}"
    ]
    where mname = text (showUnique (monitor_name m))
  ppSubgraph _ = empty
  ppHandlerNode ((_,h),_,_) = text (showUnique (handler_name h))
  ppEdge :: Edge -> Doc
  ppEdge (v1,v2) = ppHandlerNode (unvertex v1)
                <+> text "->"
                <+> ppHandlerNode (unvertex v2) <> semi

threadTriggerChan :: Thread -> Chan
threadTriggerChan (SignalThread s) = ChanSignal s
threadTriggerChan (PeriodThread p) = ChanPeriod p

towerChanHandlers :: Tower -> Chan -> [Handler]
towerChanHandlers t c = do
  m <- tower_monitors t
  monitorChanHandlers m c

monitorChanHandlers :: Monitor -> Chan -> [Handler]
monitorChanHandlers m c = filter p (monitor_handlers m)
  where p h = handler_chan h == c

handlerOutboundChans :: Handler -> [Chan]
handlerOutboundChans h = map emitter_chan (handler_emitters h)

handlerOutboundHandlers :: Tower -> Handler -> [Handler]
handlerOutboundHandlers t h = do
  c <- handlerOutboundChans h
  towerChanHandlers t c
