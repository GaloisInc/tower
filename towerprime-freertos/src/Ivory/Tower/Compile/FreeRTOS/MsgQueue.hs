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
import           Ivory.Stdlib
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Unique

import qualified Ivory.OS.FreeRTOS.Semaphore as S

data MsgQueue area =
  MsgQueue
    { mq_push :: forall eff s . ConstRef s area -> Ivory eff ()
    , mq_pop  :: forall eff s . AST.Task -> Ref s area -> Ivory eff IBool
    , mq_init :: Def('[]:->())
    , mq_code :: ModuleDef
    }

msgQueue :: forall (n :: Nat) area
          . (SingI n, IvoryArea area)
         => AST.System -> AST.Chan -> Proxy n -> MsgQueue area
msgQueue sysast chanast n = MsgQueue
  { mq_push = call_ push
  , mq_pop  = \rxertask -> call (pop rxertask)
  , mq_init = init
  , mq_code = code
  }
  where
  receiving_tasks = AST.tasks_receiving sysast chanast

  rb :: AST.Task -> RingBuffer area
  rb t = ringBuffer n (tasknamed t)

  push :: Def('[ConstRef s area]:->())
  push = proc (named "push") $ \r -> body $ do
    forM_ receiving_tasks $ \t -> ringbuffer_push (rb t) r

  pop :: AST.Task -> Def('[Ref s area]:->IBool)
  pop t = proc (tasknamed t "pop") $ \r -> body $ do
    success <- ringbuffer_pop (rb t) r
    ret success

  init = proc (named "init") $ body $ do
    forM_ receiving_tasks $ \t -> ringbuffer_init (rb t)

  code = do
    incl push
    incl init
    forM_ receiving_tasks $ \t -> do
      incl (pop t)
      ringbuffer_moddef (rb t)

  named n = "chan_" ++ (show (AST.chan_id chanast)) ++ "_" ++ n
  tasknamed t n = named (showUnique (AST.task_name t) ++ "_" ++ n)


data RingBuffer area =
  RingBuffer
    { ringbuffer_init   :: forall eff . Ivory eff ()
    , ringbuffer_push   :: forall eff s . ConstRef s area -> Ivory eff IBool
    , ringbuffer_pop    :: forall eff s .      Ref s area -> Ivory eff IBool
    , ringbuffer_moddef :: ModuleDef
    }

ringBuffer :: forall (n :: Nat) area
            . (SingI n, IvoryArea area)
           => Proxy n
           -> (String -> String)
           -> RingBuffer area
ringBuffer _ named = RingBuffer
  { ringbuffer_init   = init
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

  size :: Integer
  size = fromSing (sing :: Sing n)
  incr x = toIx ((fromIx x + 1) .% fromIntegral size)

  push v = do
    rmv  <- deref remove
    ins  <- deref insert
    ins' <- assign (incr ins)
    room <- assign (ins' /=? rmv)
    when room $ do
      refCopy (array ! ins) v
      store insert ins'
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

  init = do
    store insert 0
    store remove 0

  moddef = do
    defMemArea array_area
    defMemArea insert_area
    defMemArea remove_area

