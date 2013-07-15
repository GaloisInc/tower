{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Tower.Test.FooBarSimple where

import Ivory.Language
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
    state <- taskLocal "state"
    onPeriod 250 $ \_now -> do
      v <- deref (state ~> foo_member)
      store (state ~> foo_member) (v + 1)
      writeData fooWriter (constRef state)

barSourceTask :: (SingI n)
              => ChannelSource n (Struct "bar_state") 
              -> Task ()
barSourceTask barSource = do
    barEmitter <- withChannelEmitter barSource "barSource"
    state <- taskLocal "state"
    onPeriod 125 $ \_now -> do
      v <- deref (state ~> bar_member)
      store (state ~> bar_member) (v + 1)
      emit_ barEmitter (constRef state)

fooBarSinkTask :: (SingI n)
               => DataSink (Struct "foo_state")
               -> ChannelSink n (Struct "bar_state")
               -> Task ()
fooBarSinkTask fooSink barSink = do
  barReceiver <- withChannelReceiver barSink "barSink"
  fooReader   <- withDataReader    fooSink "fooSink"
  latestFoo   <- taskLocal "latestFoo"
  latestSum   <- taskLocal "latestSum"
  taskInit $ do
    store latestSum 0
  onChannel barReceiver $ \latestBar -> do
      readData fooReader latestFoo
      bmember <- deref (latestBar ~> bar_member)
      fmember <- deref (latestFoo ~> foo_member)
      store latestSum (bmember + fmember)

fooBarTower :: Tower ()
fooBarTower = do
  (source_f, sink_f) <- dataport
  (source_b, sink_b) <- channel

  task "fooSourceTask"  $ fooSourceTask source_f
  task "barSourceTask"  $ barSourceTask source_b
  task "fooBarSinkTask" $ fooBarSinkTask sink_f sink_b

  addDepends fooBarTypes
  addModule fooBarTypes
  addModule someOtherModule
