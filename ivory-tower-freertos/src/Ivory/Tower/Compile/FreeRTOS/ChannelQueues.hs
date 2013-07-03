{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Compile.FreeRTOS.ChannelQueues where

import GHC.TypeLits
import Text.Printf

import Ivory.Language
import Ivory.Stdlib
import qualified Ivory.OS.FreeRTOS.Queue as Q
import qualified Ivory.OS.FreeRTOS.Semaphore as S
import           Ivory.OS.FreeRTOS.Queue (QueueHandle)

import Ivory.Tower.Types
import Ivory.Tower.Compile.FreeRTOS.Types

data FreeRTOSChannel area =
  FreeRTOSChannel
    { fch_name      :: String
    , fch_emit      :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => Ctx -> ConstRef s area -> Ivory eff IBool
    , fch_emit_     :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => Ctx -> ConstRef s area -> Ivory eff ()
    , fch_receive   :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => Ctx -> Ref s area -> Ivory eff IBool
    , fch_initDef   :: Def('[]:->())
    , fch_moduleDef :: ModuleDef
    , fch_channelid :: ChannelId
    }

data FreeRTOSGuard =
  FreeRTOSGuard
    { guard_block     :: forall eff cs . (GetAlloc eff ~ Scope cs)
                      => Uint32 -> Ivory eff ()
    , guard_notify    :: forall eff . Ctx -> Ivory eff ()
    , guard_initDef   :: Def('[]:->())
    , guard_moduleDef :: ModuleDef
    }

incomingEvents :: NodeSt i -> Integer
incomingEvents n = foldl aux 0 $ map unLabeled $ nodest_receivers n
  where aux acc chid = acc +(chan_size chid)

eventGuard :: TaskNode -> FreeRTOSGuard
eventGuard node = FreeRTOSGuard
  { guard_block     = block
  , guard_notify    = notify
  , guard_initDef   = initDef
  , guard_moduleDef = moduleDef
  }
  where
  -- At least 1 to be a valid freertos primitive.
  size = max 1 $ incomingEvents node
  unique s = s ++ "_" ++ (nodest_name node)

  block :: (GetAlloc eff ~ Scope cs) => Uint32 -> Ivory eff ()
  block time = call_ blockProc time

  blockProc :: Def('[Uint32] :-> ())
  blockProc = proc (unique "guardBlock") $ \time -> body $ do
    call_ S.take guardSem time

  notify :: Ctx -> Ivory eff ()
  notify ctx = call_ (notifyProc ctx)

  notifyProc :: Ctx -> Def ('[]:->())
  notifyProc ctx = proc (unique ("guardNotifyFrom" ++ (show ctx))) $ body $
    case ctx of
      User -> call_ S.give     guardSem
      ISR  -> call_ S.give_isr guardSem

  guardSemArea :: MemArea S.Semaphore
  guardSemArea = area (unique "guardSem") Nothing
  guardSem     = addrOf guardSemArea

  initDef = proc (unique "guardInit") $ body $ do
    call_ S.create_counting guardSem (fromIntegral size) 0
    retVoid

  moduleDef = do
    incl initDef
    incl blockProc
    incl (notifyProc ISR)
    incl (notifyProc User)
    private $ defMemArea guardSemArea

eventQueue :: forall (area :: Area) (n :: Nat) i
            . (SingI n, IvoryArea area)
           => ChannelId
           -> Sing n
           -> NodeSt i -- Destination Node
           -> FreeRTOSChannel area
eventQueue channelid _sizeSing dest = FreeRTOSChannel
  { fch_name        = unique "freertos_eventQueue"
  , fch_emit        = emit
  , fch_emit_       = emit_
  , fch_receive     = receive
  , fch_initDef     = initDef
  , fch_moduleDef   = mdef
  , fch_channelid   = channelid
  }
  where
  name = printf "channel%d_%s" (chan_id channelid) (nodest_name dest)

  unique :: String -> String
  unique n = n ++ "_" ++ name

  eventHeapArea :: MemArea (Array n area)
  eventHeapArea = area (unique "eventHeap") Nothing
  eventHeap     = addrOf eventHeapArea

  pendingQueueArea, freeQueueArea :: MemArea Q.Queue
  pendingQueueArea = area (unique "pendingQueue") Nothing
  freeQueueArea    = area (unique "freeQueue") Nothing
  pendingQueue     = addrOf pendingQueueArea
  freeQueue        = addrOf freeQueueArea

  getIx :: (GetAlloc eff ~ Scope cs)
        => Ctx -> QueueHandle -> Uint32 -> Ivory eff (IBool, Ix n)
  getIx ctx q waittime = do
    vlocal <- local (ival 0)
    s <- case ctx of
           User -> call Q.receive     q vlocal waittime
           ISR  -> call Q.receive_isr q vlocal
    v <- deref vlocal
    i <- assign (toIx v)
    return (s, i)

  putIx :: Ctx -> QueueHandle -> Ix n -> Ivory eff ()
  putIx ctx q i = case ctx of
    User -> call_ Q.send     q (safeCast i) 0 -- should never block
    ISR  -> call_ Q.send_isr q (safeCast i)

  emit :: (GetAlloc eff ~ Scope cs) => Ctx -> ConstRef s area -> Ivory eff IBool
  emit ctx v = call (emitProc ctx) v

  emit_ :: (GetAlloc eff ~ Scope cs) => Ctx -> ConstRef s area -> Ivory eff ()
  emit_ ctx v = call_ (emitProc ctx) v

  emitProc :: Ctx -> Def ('[ConstRef s area] :-> IBool)
  emitProc ctx = proc (unique ("emitFrom" ++ (show ctx))) $ \v -> body $ do
    (got, i) <- getIx ctx freeQueue 0
    when got $ do
      refCopy (eventHeap ! i) v
      putIx ctx pendingQueue i
    ret got

  receive :: (GetAlloc eff ~ Scope cs) => Ctx -> Ref s area -> Ivory eff IBool
  receive ctx v = call (receiveProc ctx) v

  receiveProc :: Ctx -> Def ('[Ref s area] :-> IBool)
  receiveProc ctx =
    proc (unique ("receiveFrom" ++ (show ctx))) $ \v -> body $ do
      (got, i) <- getIx ctx pendingQueue 0
      when got $ do
        refCopy v (constRef (eventHeap ! i))
        putIx ctx freeQueue i
      ret got

  initName = unique "freertos_eventQueue_init"

  initDef :: Def ('[] :-> ())
  initDef = proc initName $ body $ do
    let len :: Uint32
        len = fromInteger (fromSing (sing :: Sing n))
    call_ Q.create pendingQueue len
    call_ Q.create freeQueue    len
    for (toIx len :: Ix n) $ \i ->
      call_ Q.send freeQueue (safeCast i) 0 -- should not bock

  mdef = do
    incl initDef
    incl (emitProc User)
    incl (emitProc ISR)
    incl (receiveProc User)
    incl (receiveProc ISR)
    private $ do
      defMemArea eventHeapArea
      defMemArea pendingQueueArea
      defMemArea freeQueueArea

