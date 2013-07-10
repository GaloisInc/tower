{-# LANGUAGE FlexibleContexts #-}
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
readData :: (GetAlloc eff ~ Scope cs, IvoryArea area)
         => TaskSchedule -> DataReader area -> Ref s area -> Ivory eff ()
readData sch reader ref = tsch_mkDataReader sch reader ref

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (GetAlloc eff ~ Scope cs, IvoryArea area)
          => TaskSchedule -> DataWriter area -> ConstRef s area -> Ivory eff ()
writeData sch writer ref = tsch_mkDataWriter sch writer ref

-- Event Loop Interface --------------------------------------------------------

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (SingI n, GetAlloc eff ~ Scope cs, IvoryArea area, IvoryZero area)
    => ChannelReceiver n area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel rxer k = EventLoop [ rx ]
  where
  rx sch = return $ do -- No initialization, pure loop handler
    ref <- local (izero)
    success <- tsch_mkReceiver sch rxer ref
    when success (k (constRef ref))

-- | like 'onChannel', but for atomic values where we don't need to handle by
--   reference.
onChannelV :: (SingI n, GetAlloc eff ~ Scope cs
              , IvoryVar t, IvoryArea (Stored t), IvoryZero (Stored t))
    => ChannelReceiver n (Stored t) -> (t -> Ivory eff ())
    -> EventLoop eff
onChannelV rxer k = EventLoop [ rx ]
  where
  rx sch = return $ do -- No initialization, pure loop handler
    ref <- local (izero)
    success <- tsch_mkReceiver sch rxer ref
    when success $ do
      v <- deref ref
      k v

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (GetAlloc eff ~ Scope cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per k = EventLoop [ \sch -> tsch_mkPeriodic sch per k ]

-- | Generate Ivory code given a 'TaskSchedule' and 'EventLoop'.
eventLoop :: ( GetAlloc eff ~ Scope cs
             , eff ~ ClearBreak (AllowBreak eff) )
          => TaskSchedule -> EventLoop eff -> Ivory eff ()
eventLoop sch el = tsch_mkEventLoop sch [ event sch | event <- unEventLoop el ]

-- Special OS function interface -----------------------------------------------

-- | Use an 'Ivory.Tower.Types.OSGetTimeMillis' implementation in an Ivory
--   monad context. We unwrap so the implementation can bind to the
--   Ivory effect scope
getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

-- Event Interface--------------------------------------------------------------

-- | Nonblocking emit. Indicates success in return value.
emit :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
   => ChannelEmitter n area -> ConstRef s area -> Ivory eff IBool
emit c r = ce_extern_emit c r
-- | Nonblocking emit. Fails silently.
emit_ :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
   => ChannelEmitter n area -> ConstRef s area -> Ivory eff ()
emit_ c r = ce_extern_emit_ c r

-- | Emit by value - saves the user from having to give a constref
--   to an atomic value.
emitV :: (SingI n, IvoryInit t, IvoryArea (Stored t), GetAlloc eff ~ Scope cs)
   => ChannelEmitter n (Stored t) -> t -> Ivory eff IBool
emitV c v = local (ival v) >>= \r -> emit c (constRef r)

emitV_ :: ( SingI n, IvoryInit t
          , IvoryArea (Stored t)
          , GetAlloc eff ~ Scope cs
          ) => ChannelEmitter n (Stored t) -> t -> Ivory eff ()
emitV_ c v = local (ival v) >>= \r -> emit_ c (constRef r)

-- | Nonblocking receive for Signals. (To receive in Tasks, use 'onChannel').
--   Indicates success in return value.
sigReceive :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
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

