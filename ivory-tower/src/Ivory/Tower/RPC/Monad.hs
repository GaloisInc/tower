{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.RPC.Monad where

import MonadLib

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.RPC.AST

newtype RPC f t a =
  RPC
    { unRPC :: StateT (RPCSt t f) (Node TaskSt) a
    } deriving (Functor, Monad)

data RPCSt f t =
  RPCSt
    { rpcst_moddef :: ModuleDef
    , rpcst_sm :: SM f t
    }

