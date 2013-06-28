{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
readData :: (Allocs eff ~ Alloc cs, IvoryArea area)
         => TaskSchedule -> DataReader area -> Ref s area -> Ivory eff ()
readData sch reader ref = tsch_mkDataReader sch reader ref

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (Allocs eff ~ Alloc cs, IvoryArea area)
          => TaskSchedule -> DataWriter area -> ConstRef s area -> Ivory eff ()
writeData sch writer ref = tsch_mkDataWriter sch writer ref

-- Event Loop Interface --------------------------------------------------------

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (SingI n, Allocs eff ~ Alloc cs, IvoryArea area, IvoryZero area)
    => ChannelReceiver n area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel rxer k = EventLoop [ rx ]
  where
  rx sch = return $ do -- No initialization, pure loop handler
    ref <- local (izero)
    success <- tsch_mkReceiver sch rxer ref
    when success (k (constRef ref))

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (Allocs eff ~ Alloc cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per k = EventLoop [ \sch -> tsch_mkPeriodic sch per k ]

-- | Generate Ivory code given a 'TaskSchedule' and 'EventLoop'.
eventLoop :: (Allocs eff ~ Alloc cs ) => TaskSchedule -> EventLoop eff -> Ivory eff ()
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
  emit :: (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
     => a -> ChannelEmitter n area -> ConstRef s area -> Ivory eff IBool
  -- | Nonblocking emit. Fails silently.
  emit_ :: (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
     => a -> ChannelEmitter n area -> ConstRef s area -> Ivory eff ()

retResult :: (a -> b -> c -> IBoolRef eff cs)
          ->  a -> b -> c -> Ivory eff IBool
retResult f s c r = do ref <- f s c r
                       failed <- deref ref
                       return (iNot failed)

instance EmitSchedulable TaskSchedule where
  emit = retResult tsch_mkEmitter
  emit_ s c r = tsch_mkEmitter s c r >> return ()

instance EmitSchedulable SigSchedule where
  emit  = retResult ssch_mkEmitter
  emit_ s c r = ssch_mkEmitter s c r >> return ()

-- | Nonblocking receive for Signals. (To receive in Tasks, use 'onChannel').
--   Indicates success in return value.
sigReceive :: (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
     => SigSchedule -> ChannelReceiver n area -> Ref s area -> Ivory eff IBool
sigReceive schedule emitter ref = ssch_mkReceiver schedule emitter ref

-- StateProxy ------------------------------------------------------------------

-- | A convenient way to transform a 'ChannelSink' to a 'DataSink' for places
--   where you only want to read the most recent value posted to a Channel,
--   rather than receive each one individually.
--   Written entirely with public API.

stateProxy :: (SingI n, IvoryArea area, IvoryZero area)
           => ChannelSink n area -> Tower (DataSink area)
stateProxy chsink = do
  (src_data, snk_data) <- dataport
  task "stateProxy" $ do
    chrxer <- withChannelReceiver chsink "proxy event"
    data_writer <- withDataWriter src_data "proxy data"
    taskBody $ \schedule ->
      eventLoop schedule $ onChannel chrxer $ \val -> do
        writeData schedule data_writer val
  return snk_data

