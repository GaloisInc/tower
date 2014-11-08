{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower
  ( Tower()
  , GeneratedCode()
  , runTower
  , ChanInput()
  , ChanOutput()
  , channel
  , signal
  , Signalable(..)
  , module Ivory.Tower.Types.Time
  , period
  , systemInit
  , Monitor()
  , monitor
  , towerModule
  , towerDepends
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.Signalable

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Codegen
import Ivory.Tower.Monad.Tower
import Ivory.Tower.Monad.Monitor

import Ivory.Language

channel :: Tower e (ChanInput a, ChanOutput a)
channel = do
  f <- fresh
  let ast = AST.SyncChan f
  towerPutASTSyncChan ast
  let c = Chan (AST.ChanSync ast)
  return (ChanInput c, ChanOutput c)

-- Note: signals are no longer tied to be the same type throughout
-- a given Tower. We'd need to add another phantom type to make that
-- work.
signal :: (Time a, Signalable s)
       => SignalType s -> a -> Tower e (ChanOutput (Stored ITime))
signal s t = do
  towerPutASTSignal ast
  towerCodegen $ codegenSignal s
  return (ChanOutput (Chan (AST.ChanSignal ast)))
  where
  n = signalName s
  ast = AST.Signal
    { AST.signal_name = n
    , AST.signal_deadline = microseconds t
    }

period :: Time a => a -> Tower e (ChanOutput (Stored ITime))
period t = do
  let ast = AST.Period (microseconds t)
  towerPutASTPeriod ast
  return (ChanOutput (Chan (AST.ChanPeriod ast)))

systemInit :: ChanOutput (Stored ITime)
systemInit = ChanOutput (Chan (AST.ChanInit AST.Init))

monitor :: String -> Monitor e () -> Tower e ()
monitor n m = do
  (ast, mcode) <- runMonitor n m
  towerPutASTMonitor ast
  towerCodegen $ codegenMonitor ast (const mcode)

towerModule :: Module -> Tower e ()
towerModule = towerCodegen . codegenModule

towerDepends :: Module -> Tower e ()
towerDepends = towerCodegen . codegenDepends

