module Ivory.Tower.Petri.Dot where


import Data.List

import Ivory.Tower.Petri.Petri
-- import Prelude


renderDotPetri :: PetriNet -> String
renderDotPetri (nodes, transitions, edges) =
  "strict digraph G {\n\r\tnewrank=true;\n\r\tgraph [overlap=false, splines=true];\n\r\t" ++
    (concat $ intersperse "\n\r\t" [renderDotNodes nodes, renderDotTransitions transitions, renderDotEdges edges] ) ++ "\n\r" ++ 
       subgraphs (nodes, transitions, edges) ++ "\n\r\t" ++(ranks (nodes, transitions, edges)) ++"\n\r}"

ranks :: PetriNet -> String
ranks (nodes, transitions, _) =
  rankNodesMonitor nodes ++ "\n\r" ++
  rankNodesHandlers nodes ++ "\n\r" ++
  rankLocks transitions ++ "\n\r"


rankNodesMonitor :: [PetriNode] -> String
rankNodesMonitor nodes =
  "\t{rank=same; " ++ 
    (concat $ intersperse "; " $ map node_name $ filter (\node -> compare "deeppink" (node_color node) == EQ ) nodes ) ++ 
    "; };"

rankNodesHandlers :: [PetriNode] -> String
rankNodesHandlers nodes =
  "\t{rank=same; " ++ 
    (concat $ intersperse "; " $ map node_name $ filter (\node -> compare "purple" (node_color node) == EQ ) nodes ) ++ 
    "; };"

rankLocks :: [PetriTransition] -> String
rankLocks transitions =
  "\t{rank=same; " ++ 
    (concat $ intersperse "; " $ map trans_name $ filter (\trans -> compare "lock_" (take 5 $ trans_name trans) == EQ ) transitions ) ++ 
    "; };"


subgraphs :: PetriNet -> String
subgraphs (nodes, transitions, edges) =
  subgraphsPeriods (nodes, transitions, edges)

subgraphsPeriods :: PetriNet -> String
subgraphsPeriods (nodes, trans, edges) =
  concat $ intersperse "\n\r" $ 
    map (makeSubgraph (nodes, trans, edges)) 
      (nub $ (map node_subgraph nodes) ++ (map trans_subgraph trans))

makeSubgraph :: PetriNet -> String -> String
makeSubgraph (nodes, trans, _) subgraph =
  "\tsubgraph cluster_" ++ subgraph ++ " {\n\r\t\tcolor = blue; \n\r\t\t" ++
    (concat $ intersperse ";\n\r\t\t" $ (nodesInSubgraph) ++ (transInSubgraph) ) ++
    ";\n\r\t\tlabel = \"" ++ subgraph ++ "\";\n\r\t}"
  where 
    nodesInSubgraph = map node_name  $ filter (\n -> compare (node_subgraph n)  subgraph == EQ) nodes
    transInSubgraph = map trans_name $ filter (\t -> compare (trans_subgraph t) subgraph == EQ) trans

renderDotNodes :: [PetriNode] -> String
renderDotNodes nodes = concat $ intersperse "\n\r\t" $ map renderDotNode nodes

renderDotNode :: PetriNode -> String
renderDotNode node = 
  node_name node ++ " [label=\""++ (node_name node) ++ "\\n" ++ 
  (replicate (node_m0 node) '•') ++"\",shape=ellipse, style=\"filled\", color="++ (node_color node) ++"];"



renderDotTransitions :: [PetriTransition] -> String
renderDotTransitions trans = concat $ intersperse "\n\r\t" $ map renderDotTransition trans

renderDotTransition :: PetriTransition -> String
renderDotTransition trans = 
  trans_name trans ++ " [label=\""++ (trans_name trans) ++"\",shape=box, style=\"filled\", color="++ (trans_color trans) ++"];"


renderDotEdges :: [PetriEdge] -> String
renderDotEdges edges = concat $ intersperse "\n\r\t" $ map renderDotEdge edges

renderDotEdge :: PetriEdge -> String
renderDotEdge edge = 
  let additionnal_label = if edge_w edge > 1 then (show $ edge_w edge) ++ "\\n" else "" in
  edge_dep edge ++ " -> " ++ (edge_arr edge) ++ " [label=\""++ additionnal_label ++ (edge_label edge) ++"\", color="++ (edge_color edge) ++"];"
