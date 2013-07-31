{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.RPC.Monad where

import MonadLib
import Data.Monoid

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.RPC.AST

newtype RPC f t a =
  RPC
    { unRPC :: WriterT (RPCSt f t) (Node TaskSt) a
    } deriving (Functor, Monad)

data RPCSt f t =
  RPCSt
    { rpcst_moddef :: ModuleDef
    , rpcst_sm :: SM f t
    }

instance Monoid (RPCSt f t) where
  mempty      = RPCSt { rpcst_moddef = return ()
                      , rpcst_sm     = mempty
                      }
  mappend a b = RPCSt { rpcst_moddef = rpcst_moddef a >> rpcst_moddef b
                      , rpcst_sm     = rpcst_sm a <> rpcst_sm b
                      }

writeSM :: SM f t -> RPC f t ()
writeSM s = RPC $ put RPCSt { rpcst_moddef = return (), rpcst_sm = s }

writeModdef :: ModuleDef -> RPC f t ()
writeModdef m = RPC $ put RPCSt { rpcst_moddef = m , rpcst_sm = mempty }

instance BaseUtils (RPC f t) where
  fresh = RPC $ lift fresh
  getOS = RPC $ lift getOS

