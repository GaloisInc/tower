{-# LANGUAGE OverloadedStrings #-}
module Ivory.Tower.AST.Chan
  ( Chan(..)
  ) where

import Text.PrettyPrint.Mainland

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.AST.Signal
import Ivory.Tower.AST.Period
import Ivory.Tower.AST.Init

data Chan
  = ChanSync   SyncChan
  | ChanSignal Signal
  | ChanPeriod Period
  | ChanInit   Init
  deriving (Eq, Show, Ord)

instance Pretty Chan where
  ppr (ChanSync c) = "Sync:" <+> ppr c
  ppr (ChanSignal s) = "Signal:" <+> ppr s
  ppr (ChanPeriod p) = "Period:" <+> ppr p
  ppr (ChanInit _i) = "Init"
