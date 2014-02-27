{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Test.TaskTest
  ( task_simple_per
  , task_simple_per_emitter
  , task_simple_per_reader
  , tower_simple_per_tasks
  ) where

import Ivory.Language
import Ivory.Tower

task_simple_per :: Task ()
task_simple_per = do
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- timerEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

task_simple_per_emitter :: ChannelSource (Stored Sint32) -> Task ()
task_simple_per_emitter c = return ()

task_simple_per_reader :: ChannelSink (Stored Sint32) -> Task ()
task_simple_per_reader c = return ()

tower_simple_per_tasks :: Tower ()
tower_simple_per_tasks = do
  task "simple_per" task_simple_per
  c <- channel
  task "simple_per_emitter" (task_simple_per_emitter (src c))
  task "simple_per_reader" (task_simple_per_reader (snk c))

