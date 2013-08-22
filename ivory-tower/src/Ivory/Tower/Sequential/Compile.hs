{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Sequential.Compile where

import Control.Monad (forM_)

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import Ivory.Tower.Sequential.AST

data Runnable =
  Runnable
    { runnable_begin   :: Def('[]:->())
    , runnable_active  :: Def('[]:->IBool)
    }

data Compiled f
  = CReceive (forall s cs 
             . ConstRef s f -> Uint32 -> Sint32 -> Ivory (AllocEffects cs) ())
  | CTime    (forall cs
             .                 Uint32 -> Sint32 -> Ivory (AllocEffects cs) ())

compileSM :: forall f t n m p
           . (IvoryArea t, IvoryArea f, IvoryZero f, SingI n, SingI m)
          => SM f t
          -> ChannelEmitter n t
          -> ChannelReceiver m f
          -> String
          -> OSGetTimeMillis
          -> Task p Runnable
compileSM sm ce cr freshn m = do
  onChannel cr $ \v -> noReturn $ rxer v m
  onPeriod  1  $ \t -> noReturn $ timerproc t
  taskModuleDef moddef
  return runnable
  where
  unique n = n ++ freshn
  runnable = Runnable
    { runnable_begin  = begin
    , runnable_active = active }

  begin = proc (unique "sequential_begin") $ body $ do
    a <- call active
    unless a $ noReturn $ do
      t <- getTimeMillis m
      ss <- mapM compileStmt (sm_start sm)
      continue 1 ss t

  active = proc (unique "sequential_active")   $ body $ do
    s <- deref state
    ret (s /=? 0)

  nextstate :: Sint32 -> Ivory eff ()
  nextstate s = store state s

  stateArea = area (unique "sequential_state") $ Just (ival 0)
  (state :: Ref Global (Stored Sint32)) = addrOf stateArea
  stateChangeArea = area (unique "sequential_state_change_time") $ Just (ival 0)
  (stateChangeTime :: Ref Global (Stored Uint32)) = addrOf stateChangeArea
  moddef = private $ do
    incl begin
    incl active
    defMemArea stateArea
    defMemArea stateChangeArea

  compiledStates = zipWith3 compileBlock fro to bbs
    where
    blocks  = sm_blocks sm
    nblocks = length blocks
    states  = cycle $ [0..nblocks]
    fro = drop 1 states -- state 0 is taken care of already
    to  = drop 1 fro -- always go one state ahead

    (bs, [final]) = splitAt (nblocks - 1) blocks
    bbs = bs ++ [ final { block_stmts = (block_stmts final) ++ (sm_end sm) } ]

  rxer v m = do
    s <- deref state
    t <- getTimeMillis m
    forM_ compiledStates $ \c -> case c of
      CReceive k -> k v t s
      _ -> return ()

  timerproc t = do
    s <- deref state
    forM_ compiledStates $ \c -> case c of
      CTime k -> k t s
      _ -> return ()

  compileBlock :: Int
               -> Int
               -> Block f t
               -> Compiled f
  compileBlock thisstate next b =
    case b of
      Block (ReceiveEvent r) stmts -> CReceive $ \rxedvalue currenttime currentstate -> do
        when ((fromIntegral thisstate) ==? currentstate) $ case r of
          ScopedRef r -> do
             refCopy r rxedvalue
             ss <- mapM compileStmt stmts
             continue next ss currenttime
      Block (TimeEvent time) stmts -> CTime $ \currenttime currentstate -> do
        when ((fromIntegral thisstate) ==? currentstate) $ do
          t <- deref stateChangeTime
          when ((currenttime - t) >? (fromIntegral time)) $ do
             ss <- mapM compileStmt stmts
             continue next ss currenttime

  continue :: Int -> [IBool] -> Uint32 -> Ivory (AllocEffects cs) ()
  continue successState ss currenttime = do
      store stateChangeTime currenttime
      anytrue <- assign $ foldr (.||) false ss
      ifte_ anytrue
        (nextstate (-1)) -- Error
        (nextstate (fromIntegral successState)) -- Continue

  compileStmt :: Stmt t
              -> Ivory (AllocEffects cs') IBool
  compileStmt s = case s of
    SLifted i -> i
    SEmit r   -> emit_ ce r >> return false



