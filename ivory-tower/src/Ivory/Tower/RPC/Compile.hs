{-# LANGUAGE ScopedTypeVariables #-}
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

compileSM :: SM f t
          -> (ConstRef s t -> Ivory (AllocEffects ss) ()) -- Emit callback
          -> String -- Fresh Name
          -> (RPCRunnable, (ConstRef s' f -> Ivory (AllocEffects ss') ()), ModuleDef)
compileSM sm emit freshname = (runnable, rxer, moddef)
  where
  unique n = n ++ freshname
  runnable = RPCRunnable
    { runnable_start  = start
    , runnable_active = active }
  start  = proc (unique "rpc_start_")  $ body $ return () -- XXX
  active = proc (unique "rpc_active_") $ body $ ret false -- XXX
  stateArea = area (unique "rpc_state_") $ Just (ival 0)
  (state :: Ref Global (Stored Uint32)) = addrOf stateArea
  moddef = private $ do
    incl start
    incl active
    defMemArea stateArea
  rxer v = return () -- XXX
