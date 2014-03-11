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

  ) where


import GHC.TypeLits

import Ivory.Language
import Ivory.Language.Area (ivoryArea)

import qualified Ivory.Tower.AST            as AST
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
  let p :: AST.System -> Def('[ConstRef s area] :-> ())
      p sys = proc (showUnique procname) $ \r -> body $ do
        OS.get_emitter os sys chan r
      mock_p :: Def('[ConstRef s area] :-> ())
      mock_p = p (error msg)

  putCommprim $ \sys _tsk -> do
    incl (p sys)

  return (ChannelEmitter (call_ mock_p))

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
  procname <- freshname pname

  putChanReceiver $ AST.ChanReceiver
    { AST.chanreceiver_name = procname
    , AST.chanreceiver_annotation = annotation
    , AST.chanreceiver_chan = chan
    }

  os <- getOS
  let p :: AST.System -> AST.Task -> Def('[Ref s area] :-> IBool)
      p sys tsk = proc (showUnique procname) $ \r -> body $ do
        success <- OS.get_receiver os sys tsk chan r
        ret success
      mock_p :: Def('[Ref s area] :-> IBool)
      mock_p = p (error msg) (error msg)

  putCommprim $ \sys tsk -> do
    incl (p sys tsk)

  return (ChannelReceiver (call mock_p))
  where
  chan = unChannelSink csnk
  pname = "receive_chan" ++ show (AST.chan_id chan)
  msg = "from Ivory.Tower.Channel.withChannelReceiver: "
     ++ "chan receive call should not be strict in OS-codegen argument"

receive :: ChannelReceiver area -> Ref s area -> Ivory eff IBool
receive = unChannelReceiver

receiveV :: (IvoryVar t, IvoryZero (Stored t), IvoryArea (Stored t), GetAlloc eff ~ Scope s)
         => ChannelReceiver (Stored t) -> Ivory eff (IBool, t)
receiveV rxer = do
  l <- local izero
  s <- receive rxer l
  v <- deref l
  return (s,v)

{-
withChannelEvent :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> Task p (ChannelEvent area)
withChannelEvent snk = undefined

withChannelLatest :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> Task p (ChannelLatest area)
withChannelLatest snk = undefined
-}

