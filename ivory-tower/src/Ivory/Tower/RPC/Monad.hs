{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.RPC.Monad where

import MonadLib

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Types
import Ivory.Tower.RPC.AST

newtype RPC p f t a =
  RPC
    { unRPC :: WriterT (SM f t) (Node TaskSt p) a
    } deriving (Functor, Monad)

runRPCMonad :: RPC p f t () -> Task p (SM f t)
runRPCMonad m = do
  (_, sm) <- runWriterT (unRPC m)
  return sm

writeSM :: SM f t -> RPC p f t ()
writeSM s = RPC $ put s

-- Public API:

rpcLocal :: (IvoryArea area) => Name -> RPC p t f (Ref Global area)
rpcLocal n = RPC $ lift $ taskLocal n

rpcLocalInit :: (IvoryArea area) => Name -> Init area -> RPC p t f (Ref Global area)
rpcLocalInit n i = RPC $ lift $ taskLocalInit n i

instance BaseUtils (RPC p f t) where
  fresh = RPC $ lift fresh
  getOS = RPC $ lift getOS

rpcStart :: [Stmt t] -> RPC p f t ()
rpcStart s = writeSM $ start s

rpcBlock :: Ref s f -> [Stmt t] -> RPC p f t ()
rpcBlock r s = writeSM $ block r s

rpcEnd :: [Stmt t] -> RPC p f t ()
rpcEnd s = writeSM $ end s


