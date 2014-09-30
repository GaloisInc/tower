{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower
  ( Tower()
  , tower
  , ChanInput()
  , ChanOutput()
  , channel
  , signal
  , module Ivory.Tower.Types.Time
  , period
  , Monitor()
  , monitor
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.TowerCode

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Handler

import Ivory.Tower.ToyObjLang

tower :: Tower () -> (AST.Tower, TowerCode)
tower = runTower

channel :: Tower (ChanInput a, ChanOutput a)
channel = do
  f <- fresh
  let ast = AST.SyncChan f
  towerPutASTSyncChan ast
  let c = Chan (AST.ChanSync ast)
  return (ChanInput c, ChanOutput c)

signal :: String -> Tower (ChanOutput ())
signal n = do
  let ast = AST.Signal n
  towerPutASTSignal ast
  return (ChanOutput (Chan (AST.ChanSignal ast)))

period :: Time a => a -> Tower (ChanOutput ITime)
period t = do
  let ast = AST.Period (microseconds t)
  towerPutASTPeriod ast
  return (ChanOutput (Chan (AST.ChanPeriod ast)))

monitor :: String -> Monitor () -> Tower ()
monitor n m = do
  a <- runMonitor n m
  towerPutASTMonitor a

