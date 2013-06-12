{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}


module Ivory.Tower.Compile.FreeRTOS.ChannelQueues where

import Text.Printf

import Ivory.Language
import Ivory.Stdlib
import qualified Ivory.OS.FreeRTOS.Queue as Q
import           Ivory.OS.FreeRTOS.Queue (QueueHandle)

import Ivory.Tower.Types

type EventQueueLen = 16
type EventQueueIx  = Ix EventQueueLen

data FreeRTOSChannel area =
  FreeRTOSChannel
    { fch_name :: String
    , fch_emit :: forall eff s cs . (eff `AllocsIn` cs)
               => ConstRef s area -> Ivory eff IBool
    , fch_receive :: forall eff s cs . (eff `AllocsIn` cs)
                  => Ref s area -> Ivory eff IBool
    , fch_initDef :: Def('[]:->())
    , fch_moduleDef :: ModuleDef
    , fch_channelid :: ChannelId
    }

data FreeRTOSGuard =
  FreeRTOSGuard
    { guard_block     :: forall eff cs . (eff `AllocsIn` cs) => Uint32 -> Ivory eff IBool
    , guard_notify    :: forall eff . Ivory eff ()
    , guard_initDef   :: Def('[]:->())
    , guard_moduleDef :: ModuleDef
    }

eventGuard :: TaskSt -> FreeRTOSGuard
eventGuard taskst = FreeRTOSGuard
  { guard_block = block
  , guard_notify = notify
  , guard_initDef = initDef
  , guard_moduleDef = moduleDef
  }
  where
  unique s = s ++ (taskst_name taskst)

  block :: (eff `AllocsIn` cs) => Uint32 -> Ivory eff IBool
  block time = do
    vlocal <- local (ival 0) -- Don't care about value rxed
    guardQueue <- addrOf guardQueueArea
    got <- call Q.receive guardQueue vlocal time
    return got

  notify :: Ivory eff ()
  notify = do
    guardQueue <- addrOf guardQueueArea
    let sentvalue = 0 -- we don't care what the value in the queue is, just its presence
        blocktime = 0 -- we don't ever want to block, and if the queue is full thats OK
    call_ Q.send guardQueue sentvalue blocktime

  guardQueueArea :: MemArea Q.Queue
  guardQueueArea = area (unique "guardQueue") Nothing

  initDef = proc (unique "freertos_guard_init_") $ body $ do
    guardQueue <- addrOf guardQueueArea
    call_ Q.create guardQueue 1 -- create queue with single element
    retVoid

  moduleDef = do
    incl initDef
    private $ defMemArea guardQueueArea

eventQueue :: forall (area :: Area). (IvoryArea area)
           => ChannelId
           -> TaskSt -- Destination Task
           -> FreeRTOSChannel area
eventQueue channelid dest = FreeRTOSChannel
  { fch_name = unique "freertos_eventQueue"
  , fch_emit = emit
  , fch_receive = receive
  , fch_initDef = initDef
  , fch_moduleDef = mdef
  , fch_channelid = channelid
  }
  where
  name = printf "channel%d_%s" (unChannelId channelid) (taskst_name dest)
  unique :: String -> String
  unique n = n ++ name
  eventHeapArea :: MemArea (Array EventQueueLen area)
  eventHeapArea = area (unique "eventHeap") Nothing
  pendingQueueArea, freeQueueArea :: MemArea Q.Queue
  pendingQueueArea = area (unique "pendingQueue") Nothing
  freeQueueArea    = area (unique "freeQueue") Nothing

  getIx :: (eff `AllocsIn` cs)
        => QueueHandle -> Uint32 -> Ivory eff (IBool, EventQueueIx)
  getIx q waittime = do
    vlocal <- local (ival 0)
    s <- call Q.receive q vlocal waittime
    v <- deref vlocal
    i <- assign (toIx v)
    return (s, i)

  putIx :: QueueHandle -> EventQueueIx -> Ivory eff ()
  putIx q i = call_ Q.send q (safeCast i) 0 -- should never block

  emit :: (eff `AllocsIn` cs) => ConstRef s area -> Ivory eff IBool
  emit v = do
    eventHeap    <- addrOf eventHeapArea
    pendingQueue <- addrOf pendingQueueArea
    freeQueue    <- addrOf freeQueueArea
    (got, i) <- getIx freeQueue 0
    when got $ do
      refCopy (eventHeap ! i) v
      putIx pendingQueue i
    return got

  receive :: (eff `AllocsIn` cs) => Ref s area -> Ivory eff IBool
  receive v = do
    eventHeap    <- addrOf eventHeapArea
    pendingQueue <- addrOf pendingQueueArea
    freeQueue    <- addrOf freeQueueArea
    (got, i) <- getIx pendingQueue 0
    when got $ do
      refCopy v (constRef (eventHeap ! i))
      putIx freeQueue i
    return got

  initName = unique "freertos_eventQueue_init"
  initDef :: Def ('[] :-> ())
  initDef = proc initName $ body $ do
    eventHeap    <- addrOf eventHeapArea
    pendingQueue <- addrOf pendingQueueArea
    freeQueue    <- addrOf freeQueueArea
    call_ Q.create pendingQueue (arrayLen eventHeap)
    call_ Q.create freeQueue    (arrayLen eventHeap)
    for (toIx (arrayLen eventHeap :: Sint32) :: EventQueueIx) $ \i ->
      call_ Q.send freeQueue (safeCast i) 0 -- should not bock

  mdef = do
    incl initDef
    private $ do
      defMemArea eventHeapArea
      defMemArea pendingQueueArea
      defMemArea freeQueueArea

