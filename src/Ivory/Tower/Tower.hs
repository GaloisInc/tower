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
  , Handler()
  , handler
  , emitter
  , callback
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.TowerCode

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Handler

tower :: Tower () -> (AST.Tower, TowerCode)
tower = runTower

channel :: Tower (ChanInput a, ChanOutput a)
channel = do
  f <- fresh
  let ast = AST.SyncChan f
  putASTSyncChan ast
  let c = Chan (AST.ChanSync ast)
  return (ChanInput c, ChanOutput c)

signal :: String -> Tower (ChanOutput ())
signal n = do
  let ast = AST.Signal n
  putASTSignal ast
  return (ChanOutput (Chan (AST.ChanSignal ast)))

period :: Time a => a -> Tower (ChanOutput ITime)
period t = do
  let ast = AST.Period (microseconds t)
  putASTPeriod ast
  return (ChanOutput (Chan (AST.ChanPeriod ast)))

monitor :: String -> Monitor () -> Tower ()
monitor n m = do
  a <- runMonitor n m
  putASTMonitor a

handler :: ChanOutput a -> String -> Handler () -> Monitor ()
handler (ChanOutput (Chan chanast)) name block = do
  ast <- runHandler name chanast block
  putASTHandler ast

emitter :: ChanInput a -> Integer -> Handler ()
emitter (ChanInput (Chan chanast)) bound = do
  putASTEmitter (AST.Emitter chanast bound)

callback :: String -> Handler ()
callback name = putASTCallback name

