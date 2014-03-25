{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Channel
  ( channel
  , channelWithSize
  , src
  , snk
  , withChannelEmitter
  , emit_
  , emitV_
  , withChannelReceiver
  , receive
  , receiveV
  , withChannelEvent
  ) where


import GHC.TypeLits

import Ivory.Language
import Ivory.Language.Area (ivoryArea)

import qualified Ivory.Tower.AST            as AST
import           Ivory.Tower.Types.Event
import qualified Ivory.Tower.Types.OS       as OS
import           Ivory.Tower.Types.Channels
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task

src :: (ChannelSource area, ChannelSink area) -> ChannelSource area
src = fst

snk :: (ChannelSource area, ChannelSink area) -> ChannelSink area
snk = snd

channel :: forall p area
         . (IvoryArea area, IvoryZero area)
        => Tower p (ChannelSource area, ChannelSink area)
channel = channelWithSize (Proxy :: Proxy 16)

channelWithSize :: forall (n :: Nat) p area
                 . (SingI n, IvoryArea area, IvoryZero area)
                => Proxy n
                -> Tower p (ChannelSource area, ChannelSink area)
channelWithSize nn = do
  cid <- fresh
  os <- getOS
  let chan = AST.Chan { AST.chan_id = cid
                      , AST.chan_size = fromSing (sing :: Sing n)
                      , AST.chan_ityp = ivoryArea (Proxy :: Proxy area)
                      }
      code astsys = OS.gen_channel os astsys chan nn (Proxy :: Proxy area)
  putChan               chan
  putSysCommInitializer ((call_ . fst) `fmap` code)
  putSysModdef          (snd `fmap` code)
  return (ChannelSource chan, ChannelSink chan)

withChannelEmitter :: forall p area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSource area
                   -> String
                   -> Task p (ChannelEmitter area)
withChannelEmitter csrc annotation = do
  procname <- freshname pname
  putChanEmitter $ AST.ChanEmitter
    { AST.chanemitter_name = procname
    , AST.chanemitter_annotation = annotation
    , AST.chanemitter_chan = chan
    }

  os <- getOS
  let pr :: AST.System p -> Def('[ConstRef s area] :-> ())
      pr sys = proc (showUnique procname) $ \r -> body $ do
        OS.get_emitter os sys chan r
      mock_pr :: Def('[ConstRef s area] :-> ())
      mock_pr = pr (error msg)

  putCommprim $ \sys -> do
    incl (pr sys)

  return (ChannelEmitter (call_ mock_pr))

  where
  chan = unChannelSource csrc
  pname = "emit_chan" ++ show (AST.chan_id chan)
  msg = "from Ivory.Tower.Channel.withChannelEmitter: "
     ++ "chan emit call should not be strict in OS-codegen argument"

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
    , evt_ast = astevt
    }
  where
  chan = unChannelSink sink
  basename t = (showUnique t) ++ "_chan_"
            ++ (show (AST.chan_id chan)) ++ "_event"
