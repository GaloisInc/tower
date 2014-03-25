
module Ivory.Tower.AST.ChanEmitter
  ( ChanEmitter(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Unique

data ChanEmitter =
  ChanEmitter
    { chanemitter_name       :: Unique
    , chanemitter_annotation :: String
    , chanemitter_chan       :: Chan
    } deriving (Eq, Show)

