
module Ivory.Tower.AST.SignalReceiver
  ( SignalReceiver(..)
  ) where

import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Unique

data SignalReceiver s =
  SignalReceiver
    { signalreceiver_name       :: Unique
    , signalreceiver_annotation :: String
    , signalreceiver_signal     :: s
    } deriving (Eq, Show)

