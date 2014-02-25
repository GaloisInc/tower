{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Channel
  ( channel
  , withChannelEmitter
  , withChannelReceiver
--  , withChannelEvent
--  , withChannelLatest
  ) where


import Ivory.Language

import qualified Ivory.Tower.AST            as AST
import qualified Ivory.Tower.Types.OS       as OS
import           Ivory.Tower.Types.Channels
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task

channel :: forall area
         . (IvoryArea area, IvoryZero area)
        => Tower (ChannelSource area, ChannelSink area)
channel = do
  cid <- fresh
  os <- getOS
  let chan = AST.Chan { AST.chan_id = cid
                      , AST.chan_size = -1 -- XXX
                      , AST.chan_ityp = undefined  -- XXX
                      }
      code astsys = OS.gen_channel os astsys chan (Proxy :: Proxy area)
  putChan               chan
  putSysCommInitializer ((call_ . fst) `fmap` code)
  putSysModdef          (snd `fmap` code)
  return (ChannelSource chan, ChannelSink chan)

withChannelEmitter :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSource area
                   -> String
                   -> Task (ChannelEmitter area)
withChannelEmitter src annotation = do
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
      mock_p = p (error msg)

  putCommprim $ \sys _tsk -> do
    incl (p sys)

  return (ChannelEmitter (call_ mock_p))

  where
  chan = unChannelSource src
  pname = "emit_chan" ++ show (AST.chan_id chan)
  msg = "from Ivory.Tower.Channel.withChannelEmitter: "
     ++ "chan emit call should not be strict in OS-codegen argument"

withChannelReceiver :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> String
                   -> Task (ChannelReceiver area)
withChannelReceiver snk annotation = do
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
      mock_p = p (error msg) (error msg)

  putCommprim $ \sys tsk -> do
    incl (p sys tsk)

  return (ChannelReceiver (call mock_p))
  where
  chan = unChannelSink snk
  pname = "receive_chan" ++ show (AST.chan_id chan)
  msg = "from Ivory.Tower.Channel.withChannelReceiver: "
     ++ "chan receive call should not be strict in OS-codegen argument"


{-
withChannelEvent :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> Task (ChannelEvent area)
withChannelEvent snk = undefined

withChannelLatest :: forall area
                    . (IvoryArea area, IvoryZero area)
                   => ChannelSink area
                   -> Task (ChannelLatest area)
withChannelLatest snk = undefined
-}

