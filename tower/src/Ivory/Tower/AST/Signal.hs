{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.Signal where

import Text.PrettyPrint.Mainland

import Ivory.Tower.Types.Time

data Signal = Signal
  { signal_name     :: String
  , signal_deadline :: Microseconds
  , signal_number   :: Int
  } deriving (Eq, Show, Ord)

instance Pretty Signal where
  ppr Signal{..} = text signal_name <> colon
              <+/> "deadline=" <> ppr signal_deadline
              <+/> "number=" <> ppr signal_number
