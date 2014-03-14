{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Frontend

task_simple_per :: Task p ()
task_simple_per = do
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- timerEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

task_simple_per_emitter :: ChannelSource (Stored Sint32) -> Task p ()
task_simple_per_emitter c = do
  e <- withChannelEmitter c "simple_emitter"
  p <- timerEvent (Milliseconds 20)
  handle p "emit_at_periodic" $ \timeRef -> do
    itime <- deref timeRef
    time <- assign (castWith 0 (toIMicroseconds itime))
    emitV_ e time


task_simple_per_reader :: ChannelSink (Stored Sint32) -> Task p ()
task_simple_per_reader c = do
  r <- withChannelReceiver c "simple_receiver"
  p <- timerEvent (Milliseconds 20)
  lastgood <- taskLocalInit "lastgood" (ival false)
  lastgot  <- taskLocal "lastgot"
  handle p "rx_at_periodic" $ \_ -> do
    (s,v) <- receiveV r
    store lastgood s
    when s $ store lastgot v

task_simple_event_reader :: ChannelSink (Stored Sint32) -> Task p ()
task_simple_event_reader c = do
  evt <- withChannelEvent c "chan_event"
  good <- taskLocalInit "good" (ival false)
  got  <- taskLocal "got"
  handle evt "rx_at_event" $ \msg -> do
    store good true
    refCopy got msg

tower_simple_per_tasks :: Tower p ()
tower_simple_per_tasks = do
  task "per_trivial" task_simple_per
  c <- channel
  task "per_emitter" (task_simple_per_emitter (src c))
  task "per_reader" (task_simple_per_reader (snk c))
  task "event_reader" (task_simple_event_reader (snk c))


main :: IO ()
main = compile defaultBuildConf tower_simple_per_tasks

