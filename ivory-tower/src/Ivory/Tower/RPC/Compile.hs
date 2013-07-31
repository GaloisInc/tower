{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.RPC.Compile where

import Ivory.Language

import Ivory.Tower.RPC.AST

data RPCRunnable =
  RPCRunnable
    { runnable_start   :: Def('[]:->())
    , runnable_active  :: Def('[]:->IBool)
    }

compileSM :: SM t f
          -> (ConstRef s t -> Ivory (AllocEffects ss) ()) -- Emit callback
          -> (RPCRunnable, (ConstRef s' f -> Ivory (AllocEffects ss') ()), ModuleDef)
compileSM = undefined

