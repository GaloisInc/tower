
module Ivory.Tower.Types.Chan
  ( Chan(..)
  ) where

import qualified Ivory.Language.Syntax as I

data Chan =
  Chan
    { chan_id :: Integer
    , chan_size :: Integer
    , chan_ityp :: I.Type
    } deriving (Eq, Show)

