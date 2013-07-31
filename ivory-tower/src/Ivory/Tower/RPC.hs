{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.RPC
  ( RPCRunnable
  , rpc
  , runRPC
  ) where

import Ivory.Language

import Ivory.Tower.Types

import Ivory.Tower.RPC.AST
import Ivory.Tower.RPC.Monad

newtype RPCRunnable = RPCRunnable { unRPCRunnable :: forall eff . Ivory eff () }

runRPC :: RPCRunnable -> Ivory eff ()
runRPC = unRPCRunnable

rpc :: ChannelSource n f -> ChannelSink m t -> RPC f t () -> Task RPCRunnable
rpc = undefined
