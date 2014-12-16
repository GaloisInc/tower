--
-- User and source location comments.
--
-- (c) 2014 Galois, Inc.
--

module Ivory.Tower.AST.Comment where

import Ivory.Tower.SrcLoc.Location

--------------------------------------------------------------------------------

data Comment = UserComment String
             | SourcePos   SrcLoc
               deriving (Show, Eq, Ord)
