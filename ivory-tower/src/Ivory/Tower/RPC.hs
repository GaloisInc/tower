{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.RPC
  ( RPCRunnable
  , rpc
  , rpcBegin
  , rpcActive
  -- AST Exports
  , Stmt
  , liftIvory
  , check
  , send
  -- Monad exports
  , RPC()
  , rpcLocal
  , rpcLocalInit
  , rpcStart
  , rpcBlock
  , rpcEnd
  ) where

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Node
import Ivory.Tower.Task
import Ivory.Tower.Ivory

import Ivory.Tower.RPC.AST
import Ivory.Tower.RPC.Compile
import Ivory.Tower.RPC.Monad

rpcBegin :: RPCRunnable -> Ivory eff ()
rpcBegin = call_ . runnable_start

rpcActive :: RPCRunnable -> Ivory eff IBool
rpcActive = call . runnable_active

rpc :: (IvoryArea f, IvoryZero f, IvoryArea t, IvoryZero t, SingI n, SingI m)
    => ChannelSource n t
    -> ChannelSink m f
    -> String
    -> RPC f t ()
    -> Task RPCRunnable
rpc source sink name m = do
  e  <- withChannelEmitter  source ("rpcTxer" ++ name)
  r  <- withChannelReceiver sink   ("rpcRxer" ++ name)
  sm <- runRPCMonad m
  n  <- freshname
  -- Need to turn SM into RPCRunnable, recieve callback, and ModuleDef (e.g. generated code)
  let (runnable, callback, moddef) = compileSM sm (emit_ e) n
  onChannel r $ \v -> do
    noReturn $ callback v
    retVoid
  taskModuleDef moddef
  return runnable

