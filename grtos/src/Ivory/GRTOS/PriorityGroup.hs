{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.PriorityGroup where

import Control.Monad (forM_)
import Data.List
import Ivory.Language

import Ivory.GRTOS.AST
import Ivory.GRTOS.Kernel

data PriorityGroup n =
  PriorityGroup
    { pg_moduledef :: ModuleDef
    , pg_pending   :: forall eff . Ivory eff IBool
    , pg_loop      :: Def('[]:->())

    , pg_wait      :: forall eff s . Ref s (Array n (Stored Uint32)) -> Ivory eff ()
    , pg_ready     :: forall eff s . Event -> Ref s (Array n (Stored Uint32)) -> Ivory eff IBool

    , pg_send      :: forall eff . Event -> Ivory eff ()
    }

priorityGroup :: forall n . (ANat n) => Priority -> PriorityGroup n
priorityGroup pri = PriorityGroup
  { pg_moduledef = md
  , pg_pending = call pending
  , pg_loop = loop
  , pg_wait = call_ wait
  , pg_ready = ready
  , pg_send = \e -> if elem e es
      then call_ (send e)
      else error ("invalid pg_send: invalid eventt " ++ show e)
  }
  where
  named n = "priority_group_" ++ (show (pri_level pri))++ "_" ++ n
  es = map fst (pri_events pri)

  md = do
    depend kernel
    defMemArea state_area
    incl loop
    incl pending
    incl wait
    mapM_ (incl . send) es

  state_area :: MemArea (Array n (Stored Uint32))
  state_area = area (named "state") Nothing
  state = addrOf state_area

  pending :: Def('[]:->IBool)
  pending = proc (named "pending") $ body $ do
    ss <- mapM (\ix -> deref (state ! (fromIntegral ix))) [0..slots]
    ret (foldl (\acc s -> (s >? 0) ? (true, acc)) false ss)


  loop :: Def('[]:->())
  loop = proc (named "eventloop") $ body $ forever $ do
    e <- local (iarray (take slots (repeat (ival 0))))
    call_ wait e
    forM_ (pri_events pri) $ \(evt, Callback cb) -> do
      r <- ready evt e
      ifte_ r cb (return ())


  slots :: Int
  slots = (n `div` 32)
    -- Should be equal to:
    -- = (fromTypeNat (aNat :: NatType n)) - 1
    where
    n =  length es

  wait :: Def('[Ref s (Array n (Stored Uint32))] :-> ())
  wait = proc (named "wait") $ \r -> body $ do
    call_ kernel_wait
    call_ kernel_begin_atomic
    refCopy r state
    mapM_ (\ix -> store (state ! (fromIntegral ix)) 0) [0..slots]
    call_ kernel_end_atomic

  ready :: Event -> Ref s (Array n (Stored Uint32)) -> Ivory eff IBool
  ready e r =  do
    s <- deref (r ! slot)
    return ((s .& (fromIntegral ((2 :: Int) ^ bitn))) >? 0)
    where
    Just eidx = elemIndex e es
    slot = fromIntegral (eidx `div` 32)
    bitn = eidx `mod` 32

  send :: Event -> Def('[]:->())
  send e = proc (named ("send_evt_" ++ (show (event_num e)))) $ body $ do
    call_ kernel_begin_atomic
    s <- deref (state ! slot)
    store (state ! slot) (s .| (fromIntegral ((2 :: Int) ^ bitn)))
    call_ kernel_end_atomic
    call_ kernel_yield
    where
    Just eidx = elemIndex e es
    slot = fromIntegral (eidx `div` 32)
    bitn = eidx `mod` 32

