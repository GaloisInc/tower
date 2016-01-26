{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Tower
import Ivory.Language
import Tower.AADL
import Ivory.Tower.Config

simpleTower :: Tower e ()
simpleTower = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  (c1in, c1out) <- channel
  (chtx, chrx) <- channel
  per <- period (Microseconds 1000)

  monitor "periodicM" $ do
    s <- state "local_st"
    handler per "tickh" $ do
      e <- emitter c1in 1
      callback $ \_ -> do
        emit e (constRef (s :: Ref 'Global ('Stored Uint8)))

  monitor "withsharedM" $ do
    s <- state "last_m2_chan1_message"

    handler c1out "fromActiveh" $ do
      e <- emitter chtx 1
      callback $ \m -> do
        refCopy s m
        emitV e true

    handler chrx "readStateh" $ do
      callback $ \_m -> do
        s' <- deref s
        call_ printf "rsh: %u\n" s'

--------------------------------------------------------------------------------

[ivory|
struct Foo { foo :: Stored Uint8 }
|]

fooMod :: Module
fooMod = package "foo" (defStruct (Proxy :: Proxy "Foo"))

simpleTower2 :: Tower e ()
simpleTower2 = do
  towerModule fooMod
  towerDepends fooMod

  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> emit e (constRef (s :: Ref 'Global ('Struct "Foo")))
      -- callback $ \_ -> emit e (constRef (s :: Ref Global (Array 3 (Stored Uint8))))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

--------------------------------------------------------------------------------

main :: IO ()
main = compileTowerAADL id p simpleTower
  where
  p topts = getConfig topts $ aadlConfigParser defaultAADLConfig

[ivory|
import (stdio.h, printf) void printf(string x, uint8_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf
