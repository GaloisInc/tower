{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Ivory where

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Tower
import Ivory.Tower.Task

-- DataPort Interface ----------------------------------------------------------

-- | Atomic read of shared data, copying to local reference. Always succeeds.
--   Takes a 'DataReader'.
readData :: (eff `AllocsIn` cs, IvoryArea area)
         => Schedule -> DataReader area -> Ref s area -> Ivory eff ()
readData sch reader ref = sch_mkDataReader sch reader ref

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (eff `AllocsIn` cs, IvoryArea area)
          => Schedule -> DataWriter area -> ConstRef s area -> Ivory eff ()
writeData sch writer ref = sch_mkDataWriter sch writer ref

-- Event Loop Interface --------------------------------------------------------

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (eff `AllocsIn` cs, IvoryArea area, IvoryZero area)
    => ChannelReceiver area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel rxer k = EventLoop [ \sch -> sch_mkReceiver sch rxer k ]

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (eff `AllocsIn` cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per k = EventLoop [ \sch -> sch_mkPeriodic sch per k ]

-- | Generate Ivory code given a 'Schedule' and 'EventLoop'.
eventLoop :: (eff `AllocsIn` cs ) => Schedule -> EventLoop eff -> Ivory eff ()
eventLoop sch el = sch_mkEventLoop sch [ event sch | event <- unEventLoop el ]

-- Special OS function interface -----------------------------------------------

-- | Use an 'Ivory.Tower.Types.OSGetTimeMillis' implementation in an Ivory
--   monad context. We unwrap so the implementation can bind to the
--   Ivory effect scope
getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

-- Event Emitter Interface -----------------------------------------------------

-- | Nonblocking emit. Fails silently.
emit :: (IvoryArea area, eff `AllocsIn` cs)
     => Schedule -> ChannelEmitter area -> ConstRef s area -> Ivory eff ()
emit schedule emitter ref = sch_mkEmitter schedule emitter ref


-- StateProxy ------------------------------------------------------------------

-- | A convenient way to transform a 'ChannelSink' to a 'DataSink' for places
--   where you only want to read the most recent value posted to a Channel,
--   rather than receive each one individually.

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

