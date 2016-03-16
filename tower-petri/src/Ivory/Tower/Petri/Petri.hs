module Ivory.Tower.Petri.Petri where

-- import Prelude


type PetriNet = ([PetriNode], [PetriTransition], [PetriEdge])
data PetriNode = PetriNode  { node_name :: String  -- name of the node
                            , node_m0 :: Int       -- initial jetons
                            , node_color :: String -- color for dot
                            , node_k :: Int        -- maximum jetons
                            , node_subgraph :: String   -- subgraph for dot
                            , node_label :: String -- label for dot
                            } deriving (Show) 

data PetriTransition = PetriTransition  { trans_name :: String     -- name of the transition
                                        , trans_color :: String    -- color for dot
                                        , trans_subgraph :: String -- subgraph for dot
                                        } deriving (Show) 
 
data PetriEdge = PetriEdge  { edge_dep :: String    -- name of the departing node/transition
                            , edge_arr :: String    -- name of the arriving  node/transition
                            , edge_w :: Int         -- cost of the transition
                            , edge_color :: String  -- color for dor
                            , edge_label :: String  -- label for the transition
                            } deriving (Show) 


petriUnion :: PetriNet -> PetriNet -> PetriNet
petriUnion (n1, t1, e1) (n2, t2, e2) = (n1 ++ n2, t1 ++ t2, e1 ++ e2)

emptyPetri :: PetriNet
emptyPetri = ([],[],[])