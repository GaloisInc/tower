module Ivory.Tower.AST.SyncChan where

import qualified Ivory.Language.Syntax.Type as I

data SyncChan = SyncChan
  { sync_chan_label :: Integer
  , sync_chan_type  :: I.Type
  } deriving (Eq, Show, Ord)

