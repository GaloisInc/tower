{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Channel
  ( channel
  , channel'
  , src
  , snk
  , withChannelEmitter
  , emit_
  , emitV_
  , withChannelReceiver
  , receive
  , receiveV
  , withChannelEvent
  , withChannelReader
  , chanRead
  , chanReadV
  ) where


import Ivory.Language
import Ivory.Language.Area (ivoryArea)

import qualified Ivory.Tower.AST            as AST
import           Ivory.Tower.Types.Event
import qualified Ivory.Tower.Types.OS       as OS
import           Ivory.Tower.Types.Channels
import           Ivory.Tower.Types.Time
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task
import           Ivory.Tower.Monad.Handler

src :: (ChannelSource area, ChannelSink area) -> ChannelSource area
src = fst

snk :: (ChannelSource area, ChannelSink area) -> ChannelSink area
snk = snd

channel :: forall p area
         . (IvoryArea area, IvoryZero area)
        => Tower p (ChannelSource area, ChannelSink area)
channel = channel' Nothing Nothing

channel' :: forall p area
                 . (IvoryArea area, IvoryZero area)
                => Maybe Microseconds -- delivery bound time in microseconds
                -> Maybe (Init area)
                -> Tower p (ChannelSource area, ChannelSink area)
channel' deliverybound initval = do
  cid <- fresh
  os <- getOS
  let ctyp = case deliverybound of
        Just i -> AST.AsynchronousChan i
        Nothing -> AST.SynchronousChan
      chan = AST.Chan { AST.chan_id = cid
                      , AST.chan_type = ctyp
                      , AST.chan_ityp = ivoryArea (Proxy :: Proxy area)
                      }
      sizenat :: Proxy 256
      sizenat = undefined
      code astsys =
        OS.gen_channel os astsys chan sizenat (Proxy :: Proxy area) initval
  putChan               chan
  putSysCommInitializer ((call_ . fst) `fmap` code)
  putSysModdef          (snd `fmap` code)
  return (ChannelSource chan, ChannelSink chan)

withChannelEmitter :: forall p n a area
                    . (ANat n, IvoryArea area, IvoryZero area)
                   => ChannelSource area
                   -> String
                   -> Proxy n
                   -> Handler p a (ChannelEmitter area)
withChannelEmitter csrc annotation bound = do
  procname <- freshname pname
  putHandlerEmitter $ AST.ChanEmitter
    { AST.chanemitter_name = procname
    , AST.chanemitter_annotation = annotation
    , AST.chanemitter_chan = chan
    , AST.chanemitter_bound = fromTypeNat bound
    }

  -- XXX GENERATE APPROPRIATE CODE:
  -- area to store messages in
  -- area to store message count in
  -- setup: initialize message count
  -- finalizer: loop over sent messages, calling all delivery procs
  --  -- XXX Need scheme for naming delivery procs, derivable from AST.System...
  --  four categories of delivery:
  --    -- write to each queue to be read by poll receiver
  --    -- write to each data area associated with a chan reader
  --    -- write to each queue associated with an asynchronous event receiver
  --    -- deliver directly to every synchronous event receiver
  --
  --    for each of these we also need to define the place code is generated for
  --    the other end. move this code out of the os-specific backend wherever
  --    possible...

  return (ChannelEmitter undefined)

  where
  chan = unChannelSource csrc
  pname = "emit_chan" ++ show (AST.chan_id chan)

emit_ :: ChannelEmitter area -> ConstRef s area -> Ivory eff ()
emit_ = unChannelEmitter

emitV_ :: (IvoryInit t, IvoryArea (Stored t), GetAlloc eff ~ Scope s)
       => ChannelEmitter (Stored t) -> t -> Ivory eff ()
emitV_ e v = do
  l <- local (ival v)
  emit_ e (constRef l)

withChannelReceiver :: forall p area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> String
                   -> Task p (ChannelReceiver area)
withChannelReceiver csnk annotation = do
  tname <- getTaskName
  pname <- freshname (basename tname)

  let chanrxer = AST.ChanReceiver
        { AST.chanreceiver_name = pname
        , AST.chanreceiver_annotation = annotation
        , AST.chanreceiver_chan = chan
        }
  putChanPollReceiver chanrxer

  os <- getOS
  let pr :: AST.System p -> Def('[Ref s area] :-> IBool)
      pr sys = proc (showUnique pname) $ \r -> body $ do
        success <- OS.get_receiver os sys chanrxer r
        ret success
      mock_pr :: Def('[Ref s area] :-> IBool)
      mock_pr = pr (error msg)

  putCommprim $ \sys -> do
    incl (pr sys)

  return (ChannelReceiver (call mock_pr))
  where
  chan = unChannelSink csnk
  msg = "from Ivory.Tower.Channel.withChannelReceiver: "
     ++ "chan receive call should not be strict in OS-codegen argument"
  basename t = (showUnique t) ++ "_chan_"
            ++ (show (AST.chan_id chan)) ++ "_receiver"

receive :: ChannelReceiver area -> Ref s area -> Ivory eff IBool
receive = unChannelReceiver

receiveV :: ( IvoryVar t, IvoryZero (Stored t), IvoryArea (Stored t)
            , GetAlloc eff ~ Scope s)
         => ChannelReceiver (Stored t) -> Ivory eff (IBool, t)
receiveV rxer = do
  l <- local izero
  s <- receive rxer l
  v <- deref l
  return (s,v)

withChannelEvent :: forall area p
                  . (IvoryArea area, IvoryZero area)
                 => ChannelSink area
                 -> String
                 -> Task p (Event area)
withChannelEvent sink annotation = do
  tname <- getTaskName
  pname <- freshname (basename tname)
  let named n = (showUnique pname) ++ "_" ++ n

  -- Write event receiver to AST:
  let chanrxer = AST.ChanReceiver
        { AST.chanreceiver_name = pname
        , AST.chanreceiver_annotation = annotation
        , AST.chanreceiver_chan = chan
        }
  putChanEventReceiver chanrxer

  -- Write channel event to AST:
  let astevt :: AST.Event
      astevt = AST.ChanEvt chan chanrxer
  putASTEvent astevt

  -- Generate Receiver code:
  os <- getOS
  let pr :: AST.System p -> Def('[Ref s area] :-> IBool)
      pr sys = proc (showUnique pname) $ \r -> body $ do
        success <- OS.get_receiver os sys chanrxer r
        ret success
      ready_area :: MemArea (Stored IBool)
      ready_area = area (named "ready") Nothing
      latest_area :: MemArea area
      latest_area = area (named "message") Nothing
  putCommprim $ \sys -> do
    incl (pr sys)
    defMemArea ready_area
    defMemArea latest_area
  putSysInitCode $ \_ ->
    store (addrOf ready_area) false
  putEventReceiverCode $ \sys -> do
    success <- call (pr sys) (addrOf latest_area)
    store (addrOf ready_area) success
  -- Return event getter code:
  return $ Event
    { evt_get = \ref -> do
        ready <- deref (addrOf ready_area)
        ifte_ ready (refCopy ref (addrOf latest_area)) (return ())
        return ready
    , evt_trigger = chantrigger astevt
    }
  where
  chan = unChannelSink sink
  basename t = (showUnique t) ++ "_chan_"
            ++ (show (AST.chan_id chan)) ++ "_event"

  chantrigger e = case AST.chan_type chan of
    AST.SynchronousChan -> AST.SynchronousTrigger chan
    AST.AsynchronousChan maxrate -> AST.AsynchronousTrigger e maxrate

withChannelReader :: forall p area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> String
                   -> Task p (ChannelReader area)
withChannelReader csnk annotation = do
  tname <- getTaskName
  pname <- freshname (basename tname)

  let chanreader = AST.ChanReader
        { AST.chanreader_name = pname
        , AST.chanreader_annotation = annotation
        , AST.chanreader_chan = chan
        }
  putChanReader chanreader

  os <- getOS
  let pr :: AST.System p -> Def('[Ref s area] :-> IBool)
      pr sys = proc (showUnique pname) $ \r -> body $ do
        success <- OS.get_reader os sys chanreader r
        ret success
      mock_pr :: Def('[Ref s area] :-> IBool)
      mock_pr = pr (error msg)

  putCommprim $ \sys -> do
    incl (pr sys)

  return (ChannelReader (call mock_pr))
  where
  chan = unChannelSink csnk
  msg = "from Ivory.Tower.Channel.withChannelReader: "
     ++ "chan read call should not be strict in OS-codegen argument"
  basename t = (showUnique t) ++ "_chan_"
            ++ (show (AST.chan_id chan)) ++ "_reader"

chanRead :: ChannelReader area -> Ref s area -> Ivory eff IBool
chanRead = unChannelReader

chanReadV :: ( IvoryVar t, IvoryZero (Stored t), IvoryArea (Stored t)
            , GetAlloc eff ~ Scope s)
         => ChannelReader (Stored t) -> Ivory eff (IBool, t)
chanReadV reader = do
  l <- local izero
  s <- chanRead reader l
  v <- deref l
  return (s,v)

