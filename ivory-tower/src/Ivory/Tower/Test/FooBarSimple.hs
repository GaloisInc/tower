{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Tower.Test.FooBarSimple where

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

fooBarTypes :: Module
fooBarTypes = package "fooBarTypes" $ do
  defStruct (Proxy :: Proxy "foo_state")
  defStruct (Proxy :: Proxy "bar_state")

barSourceTask :: (SingI n)
              => ChannelSource n (Struct "bar_state") 
              -> Task p ()
barSourceTask barSource = do
    barEmitter <- withChannelEmitter barSource "barSource"
    state <- taskLocal "state"
    onPeriod 125 $ \_now -> do
      v <- deref (state ~> bar_member)
      store (state ~> bar_member) (v + 1)
      emit_ barEmitter (constRef state)

fooBarSinkTask :: (SingI n)
               => ChannelSink n (Struct "bar_state")
               -> Task p ()
fooBarSinkTask barSink = do
  latestSum   <- taskLocal "latestSum"
  taskInit $ do
    store latestSum 0
  onChannel barSink "barSink" $ \latestBar -> do
      bmember <- deref (latestBar ~> bar_member)
      latestSum %= (+bmember)

fooBarTower :: Tower p ()
fooBarTower = do
  (source_b, sink_b) <- channel

  task "barSourceTask"  $ barSourceTask source_b
  task "fooBarSinkTask" $ fooBarSinkTask sink_b

  addDepends fooBarTypes
  addModule fooBarTypes

