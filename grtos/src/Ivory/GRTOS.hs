{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS where

import Data.List
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Compile.C.CmdlineFrontend (compile)

data Event =
  Event
    { event_num :: Int
    , event_name :: String
    } deriving (Eq, Show)

data Priority =
  Priority
    { pri_level :: Int
    , pri_events :: [Event]
    } deriving (Eq, Show)

data System = System [Priority]

---

data PriorityCode n =
  PriorityCode
    { pc_moduledef :: ModuleDef
    , pc_pending   :: forall eff . Ivory eff IBool
    , pc_wait      :: forall eff s . Ref s (Array n (Stored Uint32)) -> Ivory eff ()
    , pc_ready     :: forall eff s . Event -> Ref s (Array n (Stored Uint32)) -> Ivory eff IBool
    , pc_send      :: forall eff . Event -> Ivory eff ()
    }

kernel_wait :: Def('[]:->())
kernel_wait = proc "kernel_wait" $ body $ return ()

kernel_yield :: Def('[]:->())
kernel_yield = proc "kernel_yield" $ body $ return ()

kernel_begin_atomic :: Def('[]:->())
kernel_begin_atomic = proc "kernel_begin_atomic" $ body $ return ()

kernel_end_atomic :: Def('[]:->())
kernel_end_atomic = proc "kernel_end_atomic" $ body $ return ()

priorityCode :: forall n . (ANat n) => Priority -> PriorityCode n
priorityCode pri = PriorityCode
  { pc_moduledef = md
  , pc_pending = call pending
  , pc_wait = call_ wait
  , pc_ready = ready
  , pc_send = \e -> if elem e es
      then call_ (send e)
      else error ("invalid pc_send: invalid eventt " ++ show e)
  }
  where
  named n = "priority_group_" ++ (show (pri_level pri))++ "_" ++ n
  es = pri_events pri

  md = do
    defMemArea state_area
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

  slots :: Int
  slots = (n `div` 32)
    -- Should be equal to:
    -- = fromTypeNat (aNat :: NatType n)
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


test = compile [m]
  where
  m = package "pkg" $ pc_moduledef pc
  pc :: PriorityCode 1
  pc = priorityCode (Priority 1 es)
  es = [Event 1 "e1", Event 2 "e2"]
