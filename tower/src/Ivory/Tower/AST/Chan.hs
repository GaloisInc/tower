
module Ivory.Tower.AST.Chan
  ( Chan(..)
  , ChanType(..)
  ) where

import qualified Ivory.Language.Syntax as I
import Ivory.Tower.Types.Time

data Chan =
  Chan
    { chan_id :: Integer
    , chan_type :: ChanType
    , chan_ityp :: I.Type
    } deriving (Eq, Show)

data ChanType
  = SynchronousChan
  | AsynchronousChan Microseconds -- minimum message delivery bound in microseconds
  deriving (Eq, Show)

