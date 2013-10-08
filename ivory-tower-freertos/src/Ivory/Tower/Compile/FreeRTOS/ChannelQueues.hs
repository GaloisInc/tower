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
import qualified Ivory.OS.FreeRTOS.Semaphore as S
import qualified Ivory.OS.FreeRTOS.Atomic as A

import Ivory.Tower.Types
import Ivory.Tower.Compile.FreeRTOS.Types

data FreeRTOSChannel area =
  FreeRTOSChannel
    { fch_name      :: String
    , fch_emit      :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => ConstRef s area -> Ivory eff IBool
    , fch_emit_     :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => ConstRef s area -> Ivory eff ()
    , fch_receive   :: forall eff s cs . (GetAlloc eff ~ Scope cs)
                    => Ref s area -> Ivory eff IBool
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

eventQueue :: forall (area :: Area *) (n :: Nat) i
            . (SingI n, IvoryArea area)
           => ChannelId
           -> Sing n
           -> NodeSt i -- Destination Node
           -> FreeRTOSChannel area
eventQueue channelid sizeSing dest = if size < 2 then err else fch
  where
  (size :: Integer) = fromSing sizeSing
  err = error ("error: in channel named " ++ name ++ ":\n" ++
               "Tower FreeRTOS Channel must be of size > 1; " ++
               "channels hold one fewer event than their size")
  fch = FreeRTOSChannel
    { fch_name        = unique "freertos_eventQueue"
    , fch_emit        = emit
    , fch_emit_       = emit_
    , fch_receive     = receive
    , fch_initDef     = initDef
    , fch_moduleDef   = mdef
    , fch_channelid   = channelid
    }

  name = printf "channel%d_%s" (chan_id channelid) (nodest_name dest)

  unique :: String -> String
  unique n = n ++ "_" ++ name

  ringBufArea :: MemArea (Array n area)
  ringBufArea = area (unique "ringBuf") Nothing
  ringBuffer  = addrOf ringBufArea

  insertArea :: MemArea (Stored (Ix n))
  insertArea = area (unique "insert") (Just (ival 0))
  insert = addrOf insertArea

  removeArea :: MemArea (Stored (Ix n))
  removeArea = area (unique "remove") (Just (ival 0))
  remove = addrOf removeArea

  ovfArea :: MemArea (Stored Uint32)
  ovfArea = area (unique "ovf") (Just (ival 0))
  ovf = addrOf ovfArea
  incOvf = deref ovf >>= \o -> store ovf (o+1)

  emit :: (GetAlloc eff ~ Scope cs) => ConstRef s area -> Ivory eff IBool
  emit v = call emitProc v

  emit_ :: (GetAlloc eff ~ Scope cs) => ConstRef s area -> Ivory eff ()
  emit_ v = call_ emitProc v

  emitProc :: Def ('[ConstRef s area] :-> IBool)
  emitProc = proc (unique "emit") $ \v -> body $ do
    success <- local (ival true)
    noReturn $ A.atomic_block $ do
      rmv <- deref remove
      ins <- deref insert
      ifte_ ((ins + 1) ==? rmv) (store success false >> incOvf) $ do
        refCopy (ringBuffer ! ins) v
        store insert (ins + 1)
    deref success >>= ret

  receive :: (GetAlloc eff ~ Scope cs) => Ref s area -> Ivory eff IBool
  receive v = call receiveProc v

  receiveProc :: Def ('[Ref s area] :-> IBool)
  receiveProc = proc (unique "receive") $ \v -> body $ do
    success <- local (ival true)
    noReturn $ A.atomic_block $ do
      rmv <- deref remove
      ins <- deref insert
      ifte_ (ins ==? rmv) (store success false) $ do
        refCopy v (constRef (ringBuffer ! rmv))
        store remove (rmv + 1)
    deref success >>= ret

  initName = unique "freertos_eventQueue_init"

  initDef :: Def ('[] :-> ())
  initDef = proc initName $ body $ do
    return ()

  mdef = do
    incl initDef
    incl emitProc
    incl receiveProc
    private $ do
      defMemArea ringBufArea
      defMemArea insertArea
      defMemArea removeArea
      defMemArea ovfArea

