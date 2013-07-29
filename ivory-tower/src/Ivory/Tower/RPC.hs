{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.RPC where

import Control.Monad.Free

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower.Types
import Ivory.Tower.Monad
import Ivory.Tower.Node
import Ivory.Tower.Task


-- AST
data RPCF (t :: Area) (f :: Area) a
  = RPCLiftIvory (forall s . Ivory (ProcEffects s ()) ()) a
  | RPCCall      (Ref Global t) (Ref Global f) a -- Input, Output
  | RPCFreshName String (String -> a)
  | RPCModuleDef ModuleDef a

instance Functor (RPCF t f) where
  fmap f (RPCLiftIvory i x) = RPCLiftIvory i (f x)
  fmap f (RPCCall    i o x) = RPCCall    i o (f x)
  fmap f (RPCFreshName s k) = RPCFreshName s (f . k)
  fmap f (RPCModuleDef m x) = RPCModuleDef m (f x)

type RPC t f = Free (RPCF t f)

-- Internal interface for writing ASTs:

liftIvory :: (forall s . Ivory (ProcEffects s ()) ()) -> RPC t f ()
liftIvory i = liftF $ RPCLiftIvory i ()

rpcFreshName :: String -> RPC t f String
rpcFreshName s = liftF $ RPCFreshName s id

rpcModuleDef :: ModuleDef -> RPC t f ()
rpcModuleDef m = liftF $ RPCModuleDef m ()

rpcMkCall :: Ref Global t -> Ref Global f -> RPC t f ()
rpcMkCall i o = liftF $ RPCCall i o ()


-- Interpreter bindings to the rest of Tower.

newtype RPCSequence = RPCSequence { unRPCSequence :: Int } -- XXX needs to be a real type, integrated with the rest of tower.

rpc :: (SingI n, SingI m, IvoryArea t, IvoryArea f, IvoryZero t, IvoryZero f)
     => ChannelSource n t
     -> ChannelSink m f
     -> String
     -> RPC t f ()
     -> Task RPCSequence
rpc to fro name m = do
  emitter <- withChannelEmitter to ("rpc_to_" ++ name)
  receiver <- withChannelReceiver fro ("rpc_from_" ++ name)
  let res = run m (emptyRPC 0) -- This guy needs:
                                -- the value 'onChannel receiver'
                                -- the value 'on'
  return (undefined res) -- punting here.

-- Interpreter result. not really compelling

data RPCRes =
  RPCRes
    { rpcres_moddef   :: ModuleDef
    , rpcres_frname   :: Int
    , rpcres_states   :: forall s . [(Int, Ivory (ProcEffects s ()) ())] -- XXX this isn't really sufficient but i guess its better than nothing.
    , rpcres_state    :: Int
    }

emptyRPC :: Int -> RPCRes
emptyRPC i =
  RPCRes
    { rpcres_moddef   = return ()
    , rpcres_frname   = i
    , rpcres_states   = []
    , rpcres_state    = 0
    }

-- Interpreter

run :: RPC t f () -> RPCRes -> RPCRes
run (Pure _)                  res = res -- XXX is this correct?? or should it be emptyResult?

run (Free (RPCLiftIvory i c)) res = run c res'
  where
  res' = res { rpcres_states = (rpcres_states res) ++ [s]}
  s = (rpcres_state res, i)

run (Free (RPCCall i o c)) res = run c res'
  where
  state' = (rpcres_state res) + 1
  res' = res { rpcres_state = state' }

run (Free (RPCFreshName p k))   res = run (k name) res'
  where
  res' = res { rpcres_frname = (rpcres_frname res) + 1 }
  name = (p ++ "_rpc" ++ (show (rpcres_frname res)))

run (Free (RPCModuleDef m c)) res = run c res'
  where
  res' = res { rpcres_moddef = (rpcres_moddef res) >> m }

-- just to see if it type checks and gives us the correct name at the end of the
-- run
simple :: RPC t f ()
simple = do
  n1 <- rpcFreshName "name1"
  n2 <- rpcFreshName "name2"
  rpcModuleDef $ defMemArea (area n1 Nothing :: MemArea (Stored Uint8))
  rpcModuleDef $ defMemArea (area n2 Nothing :: MemArea (Stored Uint16))

-- Public interface should have one of these, and also expose liftIvory.

--rpcCall :: (IvoryArea t) => ConstRef s f -> RPC t f (Ref Global t)
--rpcCall ref = undefined

