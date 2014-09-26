{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower
  ( tower
  , monitor
  , channel
  , signal
  , period
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Time

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor

tower :: Tower () -> AST.Tower
tower = runTower

monitor :: String -> Monitor () -> Tower ()
monitor n m = do
  a <- runMonitor n m
  putASTMonitor a

channel :: Tower (ChanInput a, ChanOutput a)
channel = do
  f <- fresh
  let ast = AST.SyncChan f
  putASTSyncChan ast
  let c = SyncChan ast
  return (ChanInput c, ChanOutput c)

signal :: String -> Tower (ChanOutput ())
signal n = do
  let ast = AST.Signal n
  putASTSignal ast
  return (ChanOutput (SignalChan ast))

period :: Time a => a -> Tower (ChanOutput ITime)
period t = do
  let ast = AST.Period (microseconds t)
  putASTPeriod ast
  return (ChanOutput (PeriodChan ast))


