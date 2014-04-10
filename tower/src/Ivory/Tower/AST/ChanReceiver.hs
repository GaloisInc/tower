
module Ivory.Tower.AST.ChanReceiver
  ( ChanReceiver(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Unique

data ChanReceiver =
  ChanReceiver
    { chanreceiver_name       :: Unique
    , chanreceiver_annotation :: String
    , chanreceiver_chan       :: Chan
    } deriving (Eq, Show)

