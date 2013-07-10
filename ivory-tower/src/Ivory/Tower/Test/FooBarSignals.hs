{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Tower.Test.FooBarSignals where

import Data.Monoid

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower


[ivory|
struct foo_state
  { foo_member :: Stored Uint8
  }
|]

[ivory|
struct bar_state
  { bar_member :: Stored Uint8
  }
|]

[ivory|
struct some_other_type
  { some_other_member :: Stored Uint8
  }
|]

fooBarTypes :: Module
fooBarTypes = package "fooBarTypes" $ do
  defStruct (Proxy :: Proxy "foo_state")
  defStruct (Proxy :: Proxy "bar_state")

someOtherModule :: Module
someOtherModule= package "someOtherModule" $ do
  defStruct (Proxy :: Proxy "some_other_type")


fooSourceTask :: DataSource (Struct "foo_state") -> Task ()
fooSourceTask fooSource = do
    fooWriter <- withDataWriter fooSource "fooSource"
    p <- withPeriod 250
    taskModuleDef $ \_sch -> depend fooBarTypes
    taskBody $ \sch -> do
      state <- local (istruct [])
      eventLoop sch $ onTimer p $ \_now -> do
        v <- deref (state ~> foo_member)
        store (state ~> foo_member) (v + 1)
        writeData sch fooWriter (constRef state)

someSignal :: (SingI n, SingI m)
           => ChannelSource n (Stored Uint8)
           -> ChannelSink m (Struct "bar_state")
           -> Signal ()
someSignal ch1 bar = do
  chEmitter  <- withChannelEmitter ch1 "someChan"
  chReceiver <- withChannelReceiver bar "barToISR"
  signalName "isr_123"
  signalBody $ \sch -> do
    v <- local izero
    success <- sigReceive sch chReceiver v
    output <- local (ival (success ? (1,0)))
    emit_ chEmitter (constRef output)

barSourceTask :: (SingI n, SingI m)
              => ChannelSource n (Struct "bar_state") 
              -> ChannelSink m (Stored Uint8)
              -> Task ()
barSourceTask barSource chSink = do
    barEmitter <- withChannelEmitter barSource "barSource"
    p <- withPeriod 125
    c <- withChannelReceiver chSink "signalCh"
    taskModuleDef $ \_sch -> depend fooBarTypes
    taskBody $ \sch -> do
      state <- local (istruct [])
      let thandler = onTimer p $ \_now -> incrementEmit
          chandler = onChannel c $ \iref -> do
            i <- deref iref
            when (i >? 0) $ incrementEmit

          incrementEmit = do
            v <- deref (state ~> bar_member)
            store (state ~> bar_member) (v + 1)
            emit_ barEmitter (constRef state)

      eventLoop sch $ thandler <> chandler

fooBarSinkTask :: (SingI n)
               => DataSink (Struct "foo_state")
               -> ChannelSink n (Struct "bar_state")
               -> Task ()
fooBarSinkTask fooSink barSink = do
  barReceiver <- withChannelReceiver barSink "barSink"
  fooReader   <- withDataReader    fooSink "fooSink"
  taskModuleDef $ \_sch -> depend fooBarTypes
  taskBody $ \sch -> do
    latestFoo <- local (istruct [])
    latestSum <- local (ival 0)
    eventLoop sch $ onChannel barReceiver $ \latestBar -> do
      readData sch fooReader latestFoo
      bmember <- deref (latestBar ~> bar_member)
      fmember <- deref (latestFoo ~> foo_member)
      store latestSum (bmember + fmember)

fooBarTower :: Tower ()
fooBarTower = do
  (source_f, sink_f) <- dataport
  (source_b, sink_b) <- channel
  (source_i, (sink_i :: ChannelSink 2 (Stored Uint8))) <- channelWithSize

  task "fooSourceTask"  $ fooSourceTask source_f

  signal "someSignal"   $ someSignal source_i sink_b
  task "barSourceTask"  $ barSourceTask source_b sink_i
  task "fooBarSinkTask" $ fooBarSinkTask sink_f sink_b

  addDepends fooBarTypes
  addModule fooBarTypes
  addModule someOtherModule
