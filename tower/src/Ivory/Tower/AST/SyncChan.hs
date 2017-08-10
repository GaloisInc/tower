{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.SyncChan where

#if MIN_VERSION_mainland_pretty(0,6,0)
import           Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

import qualified Ivory.Language.Syntax.Type as I

data SyncChan = SyncChan
  { sync_chan_label :: Integer
  , sync_chan_type  :: I.Type
  } deriving (Eq, Show, Ord)

instance Pretty SyncChan where
  ppr SyncChan{..} =
    integer sync_chan_label <+> "::" <+> text (show sync_chan_type)
