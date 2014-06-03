{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

data ExamplePlatform = ExamplePlatform
instance Signalable ExamplePlatform where
  data SignalType ExamplePlatform = ExampleSignal1
                                  | ExampleSignal2
                                  deriving (Eq, Show, Read)
  signalName = show

task_simple_per :: Task p ()
task_simple_per = do
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- withPeriodicEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

task_simple_sig :: Task ExamplePlatform ()
task_simple_sig = do
  ctr <- taskLocal "counter"
  p <- withSignalEvent ExampleSignal1 "signal1"
  handle p "signalhandler" $ \_ -> do
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tower_simple_per_tasks :: Tower ExamplePlatform ()
tower_simple_per_tasks = do
  task "per_trivial" task_simple_per
  task "sig_trivial" task_simple_sig

main :: IO ()
main = compile defaultBuildConf tower_simple_per_tasks

test :: IO ()
test = compile' defaultBuildConf undefined undefined tower_simple_per_tasks
