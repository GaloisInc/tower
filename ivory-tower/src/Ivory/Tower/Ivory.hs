{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Ivory where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower.Types
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Node

-- DataPort Interface ----------------------------------------------------------

-- | Atomic read of shared data, copying to local reference. Always succeeds.
--   Takes a 'DataReader'.
readData :: (eff `AllocsIn` cs, IvoryArea area)
         => TaskSchedule -> DataReader area -> Ref s area -> Ivory eff ()
readData sch reader ref = tsch_mkDataReader sch reader ref

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (eff `AllocsIn` cs, IvoryArea area)
          => TaskSchedule -> DataWriter area -> ConstRef s area -> Ivory eff ()
writeData sch writer ref = tsch_mkDataWriter sch writer ref

-- Event Loop Interface --------------------------------------------------------

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (eff `AllocsIn` cs, IvoryArea area, IvoryZero area)
    => ChannelReceiver area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel rxer k = EventLoop [ rx ]
  where
  rx sch = return $ do -- No initialization, pure loop handler
    ref <- local (izero)
    success <- tsch_mkReceiver sch rxer ref
    when success (k (constRef ref))

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (eff `AllocsIn` cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per k = EventLoop [ \sch -> tsch_mkPeriodic sch per k ]

-- | Generate Ivory code given a 'TaskSchedule' and 'EventLoop'.
eventLoop :: (eff `AllocsIn` cs ) => TaskSchedule -> EventLoop eff -> Ivory eff ()
eventLoop sch el = tsch_mkEventLoop sch [ event sch | event <- unEventLoop el ]

-- Special OS function interface -----------------------------------------------

-- | Use an 'Ivory.Tower.Types.OSGetTimeMillis' implementation in an Ivory
--   monad context. We unwrap so the implementation can bind to the
--   Ivory effect scope
getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

-- Event Interface--------------------------------------------------------------

class EmitSchedulable a where
  -- | Nonblocking emit. Indicates success in return value.
  emit :: (IvoryArea area, eff `AllocsIn` cs)
     => a -> ChannelEmitter area -> ConstRef s area -> Ivory eff IBool
  -- | Nonblocking emit. Fails silently.
  emit_ :: (IvoryArea area, eff `AllocsIn` cs)
     => a -> ChannelEmitter area -> ConstRef s area -> Ivory eff ()
  emit_ s c r = emit s c r >> return () 

instance EmitSchedulable TaskSchedule where
  emit schedule emitter ref = tsch_mkEmitter schedule emitter ref

instance EmitSchedulable SigSchedule where
  emit schedule emitter ref = ssch_mkEmitter schedule emitter ref

-- | Nonblocking receive for Signals. (To receive in Tasks, use 'onChannel').
--   Indicates success in return value.
sigReceive :: (IvoryArea area, eff `AllocsIn` cs)
     => SigSchedule -> ChannelReceiver area -> Ref s area -> Ivory eff IBool
sigReceive schedule emitter ref = ssch_mkReceiver schedule emitter ref

-- StateProxy ------------------------------------------------------------------

-- | A convenient way to transform a 'ChannelSink' to a 'DataSink' for places
--   where you only want to read the most recent value posted to a Channel,
--   rather than receive each one individually.
--   Written entirely with public API.

stateProxy :: (IvoryArea area, IvoryZero area)
           => ChannelSink area -> Tower (DataSink area)
stateProxy chsink = do
  (src_data, snk_data) <- dataport
  task "stateProxy" $ do
    chrxer <- withChannelReceiver chsink "proxy event"
    data_writer <- withDataWriter src_data "proxy data"
    taskBody $ \schedule ->
      eventLoop schedule $ onChannel chrxer $ \val -> do
        writeData schedule data_writer val
  return snk_data

