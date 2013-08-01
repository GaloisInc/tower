{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.RPC.Compile where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower.RPC.AST

data RPCRunnable =
  RPCRunnable
    { runnable_begin   :: Def('[]:->())
    , runnable_active  :: Def('[]:->IBool)
    }

compileSM :: forall f t . (IvoryArea f)
          => SM f t
          -> (forall s cs . ConstRef s t -> Ivory (AllocEffects cs) ())
          -> String
          -> ( RPCRunnable
             , (forall s' cs' . ConstRef s' f -> Ivory (AllocEffects cs') ())
             , ModuleDef )
compileSM sm emitter freshname = (runnable, rxer, moddef)
  where
  unique n = n ++ freshname
  runnable = RPCRunnable
    { runnable_begin  = begin
    , runnable_active = active }

  begin = proc (unique "rpc_begin") $ body $ do
    a <- call active
    unless a $ noReturn $ do
      compileStmts 1 (sm_start sm)

  active = proc (unique "rpc_active")   $ body $ do
    s <- deref state
    ret (s /=? 0)

  nextstate :: Sint32 -> Ivory eff ()
  nextstate s = store state s

  stateArea = area (unique "rpc_state") $ Just (ival 0)
  (state :: Ref Global (Stored Sint32)) = addrOf stateArea
  moddef = private $ do
    incl begin
    incl active
    defMemArea stateArea

  rxer v = do
    s <- deref state
    sequence_ $ zipWith3 (compileBlock s v) fro to bbs
    where
    blocks  = sm_blocks sm
    nblocks = length blocks
    states  = cycle $ [0..nblocks]
    fro = drop 1 states -- state 0 is taken care of already
    to  = drop 1 fro -- always go one state ahead

    (bs, [final]) = splitAt (nblocks - 1) blocks
    bbs = bs ++ [ final { block_stmts = (block_stmts final) ++ (sm_end sm) } ]

  compileBlock :: Sint32
               -> ConstRef s' f
               -> Int
               -> Int
               -> Block f t
               -> Ivory (AllocEffects cs) ()
  compileBlock currentstate rxedvalue thisstate nextstate b = do
    when ((fromIntegral thisstate) ==? currentstate) $ case b of
       Block r ss -> do
         refCopy r rxedvalue
         compileStmts nextstate ss

  compileStmts ::Int
              -> [Stmt t]
              -> Ivory (AllocEffects cs') ()
  compileStmts successState stmts = do
      ss <- mapM compileStmt stmts
      anytrue <- assign $ foldr (.||) false ss
      ifte_ anytrue
        (nextstate (-1)) -- Error
        (nextstate (fromIntegral successState)) -- Continue

  compileStmt :: Stmt t
              -> Ivory (AllocEffects cs') IBool
  compileStmt s = case s of
    SLifted i -> i
    SEmit r   -> emitter r >> return false


