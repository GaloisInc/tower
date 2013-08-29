{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.Test.StateMachine where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine

[ivory|
struct foo
  { foo_member :: Stored Uint8
  }
|]

[ivory|
struct bar
  { bar_member :: Stored Uint8
  }
|]

fooBarTypes :: Module
fooBarTypes = package "fooBarTypes" $ do
  defStruct (Proxy :: Proxy "foo")
  defStruct (Proxy :: Proxy "bar")

client :: (SingI n, SingI m)
       => ChannelSource n (Struct "foo")
       -> ChannelSink   m (Struct "bar")
       -> Task p ()
client t f = do
  send1  <- taskLocal "send1"
  send2  <- taskLocal "send2"
  got1   <- taskLocal "got1"
  got2   <- taskLocal "got2"
  gotSum <- taskLocal "gotSum"

  rx <- withChannelEvent f "from"
  tx <- withChannelEmitter t "to"

  runner <- stateMachine "testClient" $ mdo
    s1 <- state $ timeout 0 $ do
      liftIvory_ $ emit_ tx (constRef send1)
      goto s2
    s2 <- state $ on rx $ \v -> do
      liftIvory_ $ do
        deref (v ~> bar_member) >>= (store got1)
        emit_ tx (constRef send2)
      goto s3
    s3 <- state $ on rx $ \v -> do
      liftIvory_ $ deref (v ~> bar_member) >>= (store got2)
      goto s4
    s4 <- state $ timeout 125 $ do
      liftIvory $ do
        r1 <- deref got1
        r2 <- deref got2
        store gotSum (r1 + r2)
        return $ do
          branch (r1 >? 100) s1
          halt
    return s1

  onPeriod 2000 $ \_time -> do
    a <- active runner
    unless a $ do
      store (send1 ~> foo_member) 10
      store (send2 ~> foo_member) 33
      begin runner

-- Server adds one to a number, but only returns the result at
-- a fixed 250ms period boundary.
server :: (SingI n, SingI m)
       => ChannelSink   n (Struct "foo")
       -> ChannelSource m (Struct "bar")
       -> Task p ()
server istream outCh = do
  ostream  <- withChannelEmitter  outCh "ostream"
  tosend   <- taskLocalInit "tosend" (ival false)
  lastrxed <- taskLocal "lastrxed"
  err      <- taskLocalInit "error" (ival 0)
  onPeriod 250 $ \_time -> do
    p <- deref tosend
    when p $ do
      got <- deref (lastrxed ~> foo_member)
      out <- local (istruct [ bar_member .= ival (got + 1) ])
      emit_ ostream (constRef out)
      store tosend false
  onChannel istream "istream" $ \v -> do
    full <- deref tosend
    unless full $ do
      refCopy lastrxed v
      store tosend true
    -- Otherwise, drop messages and log an error.
    when full $ do
      (e :: Uint32) <- deref err
      store err (e + 1)

stateMachineTestTower :: Tower p ()
stateMachineTestTower = do
  callCh <- channel
  respCh <- channel
  task "client" $ client (src callCh) (snk respCh)
  task "server" $ server (snk callCh) (src respCh)

  addDepends fooBarTypes
  addModule fooBarTypes
