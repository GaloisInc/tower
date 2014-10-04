
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

data MessageSource = ThreadMessage Thread
                   | HandlerMessage Monitor Handler
                   deriving (Eq, Show, Ord)

handlerMessageSource :: (Monitor, Handler) -> MessageSource
handlerMessageSource = uncurry HandlerMessage

handlerGraph :: Tower
             -> ( Graph
                , (Vertex -> (MessageSource, MessageSource, [MessageSource]))
                , MessageSource-> Maybe Vertex)
handlerGraph t = graphFromEdges (handleredges ++ threadedges)
  where
  threadedges = do
    th <- towerThreads t
    let ms = ThreadMessage th
        ch = threadChan th
    return (ms, ms, map handlerMessageSource (towerChanHandlers t ch))
  handleredges = do
    m <- tower_monitors t
    h <- monitor_handlers m
    let ms = HandlerMessage m h
    return (ms, ms, map handlerMessageSource (handlerOutboundHandlers t h))

graphviz :: ( Graph
            , (Vertex -> (MessageSource, MessageSource, [MessageSource]))
            , MessageSource -> Maybe Vertex
            )
            -> String
graphviz (g, unvertex, _) = pretty 80 $ stack $
  [ text "digraph Tower {"
  , indent 4 $ stack $ map ppSubgraph monitors
  , empty
  , indent 4 (stack (map ppEdge (edges g)))
  , text "}"
  ]
  where
  eqmonitor ((ThreadMessage t1),_,_) ((ThreadMessage t2),_,_) = t1 == t2
  eqmonitor ((HandlerMessage m1 _),_,_) ((HandlerMessage m2 _),_,_) = m1 == m2
  eqmonitor _ _ = False
  monitors = groupBy eqmonitor (map unvertex (vertices g))
  ppSubgraph ms@(((HandlerMessage m _),_,_):_) = stack
    [ text "subgraph " <+> text "cluster_" <> mname <+> text "{"
    , indent 4 $ text "color = blue" <> semi
    , indent 4 $ text "node [style=filled]" <> semi
    , indent 4 $ stack $ map (\h -> ppHandlerNode h <> semi) ms
    , indent 4 $ text "label =" <+> dquotes (text "monitor" <+> mname) <> semi
    , text "}"
    ]
    where mname = text (showUnique (monitor_name m))
  ppSubgraph ([((ThreadMessage t),_,_)]) =
    tname <+> text "[style=filled]" <> semi
    where tname = text (threadName t)

  ppSubgraph _ = empty -- XXX
  ppHandlerNode ((HandlerMessage _ h),_,_) = text (showUnique (handler_name h))
  ppHandlerNode ((ThreadMessage t),_,_) = text (threadName t)
  ppEdge :: Edge -> Doc
  ppEdge (v1,v2) = ppHandlerNode (unvertex v1)
                <+> text "->"
                <+> ppHandlerNode (unvertex v2) <> semi

towerChanHandlers :: Tower -> Chan -> [(Monitor, Handler)]
towerChanHandlers t c = do
  m <- tower_monitors t
  h <- monitorChanHandlers m c
  return (m, h)

monitorChanHandlers :: Monitor -> Chan -> [Handler]
monitorChanHandlers m c = filter p (monitor_handlers m)
  where p h = handler_chan h == c

handlerOutboundChans :: Handler -> [Chan]
handlerOutboundChans h = map emitter_chan (handler_emitters h)

handlerOutboundHandlers :: Tower -> Handler -> [(Monitor, Handler)]
handlerOutboundHandlers t h = do
  c <- handlerOutboundChans h
  towerChanHandlers t c
