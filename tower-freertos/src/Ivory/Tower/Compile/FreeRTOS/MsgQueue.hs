{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Compile.FreeRTOS.MsgQueue
  ( MsgQueue(..)
  , msgQueue
  ) where

import           Control.Monad (forM_)
import           GHC.TypeLits
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Unique

import           Ivory.Tower.Compile.FreeRTOS.EventNotify
import qualified Ivory.OS.FreeRTOS.Mutex as M

data MsgQueue area =
  MsgQueue
    { mq_push :: forall eff s . ConstRef s area -> Ivory eff ()
    , mq_pop  :: forall eff s . AST.ChanReceiver -> Ref s area -> Ivory eff IBool
    , mq_read :: forall eff s . Ref s area -> Ivory eff IBool
    , mq_init :: Def('[]:->())
    , mq_code :: ModuleDef
    }

msgQueue :: forall (n :: Nat) area p
          . (ANat n, IvoryArea area)
         => AST.System p -> AST.Chan -> Proxy n -> Maybe (Init area)
         -> MsgQueue area
msgQueue sysast chanast n initval = MsgQueue
  { mq_push = call_ push
  , mq_pop  = \chanrxer -> call (pop chanrxer)
  , mq_read = call mread
  , mq_init = ini
  , mq_code = code
  }
  where
  event_receivers = AST.event_receivers sysast chanast
  poll_receivers = AST.poll_receivers sysast chanast
  all_receivers  = event_receivers ++ poll_receivers

  rb :: AST.ChanReceiver -> RingBuffer area
  rb t = ringBuffer n (rxernamed t)

  push :: Def('[ConstRef s area]:->())
  push = proc (named "push") $ \r -> body $ do
    call_ M.take mutex_ref
    forM_ all_receivers $ \(rxer, _) ->
      ringbuffer_push (rb rxer) r
    refCopy read_buffer r
    store read_valid true
    call_ M.give mutex_ref
    forM_ event_receivers $ \(_, taskast) ->
      evtn_trigger (taskEventNotify (AST.task_name taskast))

  pop :: AST.ChanReceiver -> Def('[Ref s area]:->IBool)
  pop t = proc (rxernamed t "pop") $ \r -> body $ do
    call_ M.take mutex_ref
    success <- ringbuffer_pop (rb t) r
    call_ M.give mutex_ref
    ret success

  mread :: Def('[Ref s area]:->IBool)
  mread = proc (named "read") $ \r -> body $ do
    call_ M.take mutex_ref
    valid <- deref read_valid
    when valid $ refCopy r read_buffer
    call_ M.give mutex_ref
    ret valid

  read_buffer_area :: MemArea area
  read_buffer_area = area (named "read_buffer") initval
  read_buffer = addrOf read_buffer_area
  read_valid_area :: MemArea (Stored IBool)
  read_valid_area = area (named "read_valid") Nothing
  read_valid = addrOf read_valid_area

  ini = proc (named "init") $ body $ do
    store read_valid (maybe false (const true) initval)
    call_ M.create mutex_ref
    forM_ all_receivers $ \(rxer, _) -> ringbuffer_init (rb rxer)

  mutex_area :: MemArea (Stored M.Mutex)
  mutex_area = area (named "mutex") Nothing
  mutex_ref :: M.MutexHandle
  mutex_ref = addrOf mutex_area

  code = do
    incl push
    incl ini
    incl mread
    defMemArea mutex_area
    defMemArea read_buffer_area
    defMemArea read_valid_area
    forM_ all_receivers $ \(rxer, _) -> do
      incl (pop rxer)
      ringbuffer_moddef (rb rxer)

  named nn = "chan_" ++ (show (AST.chan_id chanast)) ++ "_" ++ nn
  rxernamed rx nn = named (showUnique (AST.chanreceiver_name rx) ++ "_" ++ nn)


data RingBuffer area =
  RingBuffer
    { ringbuffer_init   :: forall eff . Ivory eff ()
    , ringbuffer_push   :: forall eff s . ConstRef s area -> Ivory eff IBool
    , ringbuffer_pop    :: forall eff s .      Ref s area -> Ivory eff IBool
    , ringbuffer_moddef :: ModuleDef
    }

ringBuffer :: forall (n :: Nat) area
            . (ANat n, IvoryArea area)
           => Proxy n
           -> (String -> String)
           -> RingBuffer area
ringBuffer _ named = RingBuffer
  { ringbuffer_init   = ini
  , ringbuffer_push   = push
  , ringbuffer_pop    = pop
  , ringbuffer_moddef = moddef
  }
  where
  array_area :: MemArea (Array n area)
  array_area = area (named "array") Nothing
  array = addrOf array_area

  insert_area :: MemArea (Stored (Ix n))
  insert_area = area (named "insert") Nothing
  insert = addrOf insert_area

  remove_area :: MemArea (Stored (Ix n))
  remove_area = area (named "remove") Nothing
  remove = addrOf remove_area

  dropped_area :: MemArea (Stored Uint32)
  dropped_area = area (named "dropped") Nothing
  dropped = addrOf dropped_area

  size :: Integer
  size = fromTypeNat (aNat :: NatType n)
  incr x = toIx ((fromIx x + 1) .% fromIntegral size)

  push v = do
    rmv  <- deref remove
    ins  <- deref insert
    ins' <- assign (incr ins)
    room <- assign (ins' /=? rmv)
    ifte_ room
      (do refCopy (array ! ins) v
          store insert ins')
      (dropped %= (+1))
    return room

  pop v = do
    rmv  <- deref remove
    rmv' <- assign (incr rmv)
    ins  <- deref insert
    nonempty <- assign (ins /=? rmv)
    when nonempty $ do
      refCopy v (constRef (array ! rmv))
      store remove rmv'
    return nonempty

  ini = do
    store insert 0
    store remove 0

  moddef = do
    defMemArea array_area
    defMemArea insert_area
    defMemArea remove_area
    defMemArea dropped_area

