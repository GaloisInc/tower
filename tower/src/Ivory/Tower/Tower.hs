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
import Ivory.Tower.Codegen.System

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor

import Ivory.Language

tower :: Tower () -> (AST.Tower, [Module])
tower t = (ast, generatedCodeModules gc ast)
  where (ast, gc) = runTower t

channel :: Tower (ChanInput a, ChanOutput a)
channel = do
  f <- fresh
  let ast = AST.SyncChan f
  towerPutASTSyncChan ast
  let c = Chan (AST.ChanSync ast)
  return (ChanInput c, ChanOutput c)

signal :: String -> Tower (ChanOutput (Stored ITime))
signal n = do
  let ast = AST.Signal n
  towerPutASTSignal ast
  return (ChanOutput (Chan (AST.ChanSignal ast)))

period :: Time a => a -> Tower (ChanOutput (Stored ITime))
period t = do
  let ast = AST.Period (microseconds t)
  towerPutASTPeriod ast
  return (ChanOutput (Chan (AST.ChanPeriod ast)))

monitor :: String -> Monitor () -> Tower ()
monitor n m = do
  (ast, mcode) <- runMonitor n m
  towerPutASTMonitor ast
  towerCodegen $ codegenMonitor ast (const mcode)

