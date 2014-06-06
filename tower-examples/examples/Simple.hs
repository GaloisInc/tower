{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Frontend
import Data.String (fromString)

-- Expected C function prototype: void printString(const char *);
print_string :: Def('[IString]:->())
print_string = externProc "printString"

-- Expected C function prototype: void printUint32(uint32_t);
print_uint32 :: Def('[Uint32]:->())
print_uint32 = externProc "printUint32"

print_deps :: Task p ()
print_deps = taskModuleDef (inclHeader "print_helpers.h")

task_simple_per :: Task p ()
task_simple_per = do
  print_deps
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- withPeriodicEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Uint32) -> store ctr (c + 1)
    call_ print_string $ fromString "Counter = "
    deref ctr >>= call_ print_uint32

task_simple_per_emitter :: ChannelSource (Stored Sint32) -> Task p ()
task_simple_per_emitter c = do
  e <- withChannelEmitter c "simple_emitter"
  p <- withPeriodicEvent (Milliseconds 20)
  handle p "emit_at_periodic" $ \timeRef -> do
    itime <- deref timeRef
    time <- assign (castWith 0 (toIMicroseconds itime))
    emitV_ e time


task_simple_per_receiver :: ChannelSink (Stored Sint32) -> Task p ()
task_simple_per_receiver c = do
  r <- withChannelReceiver c "simple_receiver"
  p <- withPeriodicEvent (Milliseconds 20)
  lastgood <- taskLocalInit "lastgood" (ival false)
  lastgot  <- taskLocal "lastgot"
  handle p "rx_at_periodic" $ \_ -> do
    (s,v) <- receiveV r
    store lastgood s
    when s $ store lastgot v

task_simple_event_handler :: ChannelSink (Stored Sint32) -> Task p ()
task_simple_event_handler c = do
  evt <- withChannelEvent c "chan_event"
  good <- taskLocalInit "good" (ival false)
  got  <- taskLocal "got"
  handle evt "rx_at_event" $ \msg -> do
    store good true
    refCopy got msg

task_simple_per_reader :: ChannelSink (Stored Sint32) -> Task p ()
task_simple_per_reader c = do
  reader <- withChannelReader c "chan_reader"
  good <- taskLocalInit "good" (ival false)
  got  <- taskLocal "got"
  p <- withPeriodicEvent (Milliseconds 20)
  handle p "read_at_periodic" $ \_ -> do
    s <- chanRead reader got
    store good s

tower_simple_per_tasks :: Tower NoSignals ()
tower_simple_per_tasks = do
  task "per_trivial" task_simple_per
  c <- channel
  task "per_emitter" (task_simple_per_emitter (src c))
  task "per_receiver" (task_simple_per_receiver (snk c))
  task "event_handler" (task_simple_event_handler(snk c))

  task "per_reader" (task_simple_per_reader  (snk c))


main :: IO ()
main = compile defaultBuildConf tower_simple_per_tasks

