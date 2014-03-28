
module Ivory.Tower.AST.ChanReader
  ( ChanReader(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Unique

data ChanReader =
  ChanReader
    { chanreader_name       :: Unique
    , chanreader_annotation :: String
    , chanreader_chan       :: Chan
    } deriving (Eq, Show)

