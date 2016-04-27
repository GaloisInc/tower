{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.SyncChan where

import Text.PrettyPrint.Mainland

import qualified Ivory.Language.Syntax.Type as I

data SyncChan = SyncChan
  { sync_chan_label :: Integer
  , sync_chan_type  :: I.Type
  } deriving (Eq, Show, Ord)

instance Pretty SyncChan where
  ppr SyncChan{..} =
    integer sync_chan_label <+> "::" <+> text (show sync_chan_type)
