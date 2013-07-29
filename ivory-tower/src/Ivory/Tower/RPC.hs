{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.RPC where

import Control.Monad.Free

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Node
import Ivory.Tower.Task ()
import Ivory.Tower.Ivory


-- AST
data RPCF (t :: Area) (f :: Area) a
  = RPCLiftIvory (forall s . Ivory (ProcEffects s ()) ()) a
  | RPCEmit      (forall s . ConstRef s t) a
  | RPCReceive   (Ref Global f) a
  | RPCFreshName String (String -> a)
  | RPCModuleDef ModuleDef a

instance Functor (RPCF t f) where
  fmap f (RPCLiftIvory i x) = RPCLiftIvory i (f x)
  fmap f (RPCEmit      i x) = RPCEmit      i (f x)
  fmap f (RPCReceive   o x) = RPCReceive   o (f x)
  fmap f (RPCFreshName s k) = RPCFreshName s (f . k)
  fmap f (RPCModuleDef m x) = RPCModuleDef m (f x)

type RPC t f = Free (RPCF t f)

-- Internal interface for writing ASTs:

rpcLiftIvory :: (forall s . Ivory (ProcEffects s ()) ()) -> RPC t f ()
rpcLiftIvory i = liftF $ RPCLiftIvory i ()

rpcFreshName :: String -> RPC t f String
rpcFreshName s = liftF $ RPCFreshName s id

rpcModuleDef :: ModuleDef -> RPC t f ()
rpcModuleDef m = liftF $ RPCModuleDef m ()

rpcEmit :: (forall s . ConstRef s t) -> RPC t f ()
rpcEmit i = liftF $ RPCEmit i ()

rpcReceive :: Ref Global f -> RPC t f ()
rpcReceive o = liftF $ RPCReceive o ()

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
  let ctx = CgenCtx { ctx_emit = emit_ emitter }
      res = cgen ctx m (emptyRPC 0)
  return (undefined res) -- punting here.

-- Interpreter result. not really compelling

data RPCRes f =
  RPCRes
    { rpcres_moddef   :: ModuleDef
    , rpcres_frname   :: Int
    , rpcres_receivers :: forall s s' . [(Int, ConstRef s' f -> Ivory (ProcEffects s ()) ())]
    , rpcres_state     :: Int
    }

emptyRPC :: Int -> RPCRes f
emptyRPC i =
  RPCRes
    { rpcres_moddef    = return ()
    , rpcres_frname    = i
    , rpcres_receivers = []
    , rpcres_state     = 0
    }

data CgenCtx (t :: Area) (f :: Area) =
  CgenCtx
    { ctx_emit :: forall s s' . ConstRef s' t -> Ivory (ProcEffects s ()) ()
    }

-- Interpreter

cgen :: CgenCtx t f -> RPC t f () -> RPCRes f -> RPCRes f
cgen _   (Pure _)                  res = res -- XXX is this correct?? or should it be emptyResult?

cgen ctx (Free (RPCLiftIvory i c)) res = cgen ctx c res'
  where
  res' = res { rpcres_receivers = (rpcres_receivers res) ++ [s]}
  s = (rpcres_state res, const i)

cgen ctx (Free (RPCEmit      i c)) res = cgen ctx c res' -- XXX
  where
  res' = res { rpcres_receivers = (rpcres_receivers res) ++ [s]}
  s = (rpcres_state res, const $ ctx_emit ctx i)

cgen ctx (Free (RPCReceive   o c)) res = cgen ctx c res'
  where
  -- Actually need to generate code here
  state' = (rpcres_state res) + 1
  res' = res { rpcres_state = state' }

cgen ctx (Free (RPCFreshName p k))   res = cgen ctx (k name) res'
  where
  res' = res { rpcres_frname = (rpcres_frname res) + 1 }
  name = (p ++ "_rpc" ++ (show (rpcres_frname res)))

cgen ctx (Free (RPCModuleDef m c)) res = cgen ctx c res'
  where
  res' = res { rpcres_moddef = (rpcres_moddef res) >> m }

-- just to see if it type checks and gives us the correct name at the end of the
-- run
simple :: RPC t f ()
simple = do
  n1 <- rpcFreshName "name1"
  n2 <- rpcFreshName "name2"
  let mem1 = area n1 Nothing :: MemArea (Stored Uint8)
      ref1 = addrOf mem1
      mem2 = area n2 Nothing :: MemArea (Stored Uint16)
      ref2 = addrOf mem2
  rpcModuleDef $ defMemArea mem1
  rpcModuleDef $ defMemArea mem2
  rpcLiftIvory $ store ref1 1
  rpcLiftIvory $ store ref2 2

-- Public interface should have one of these, and also expose liftIvory.

--rpcCall :: (IvoryArea t) => ConstRef s f -> RPC t f (Ref Global t)
--rpcCall ref = undefined

