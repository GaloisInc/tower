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
    handler per "send" $ do
      e <- emitter c1in 1
      callback $ \_ -> do
        emit e (constRef (s :: Ref 'Global ('Stored Uint8)))
    handler chrx "rcv" $ callback $ \msg -> do
      n' <- deref msg
      store s (n' + 1)
      call_ printf "received: %u\n" n'

{-
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
-}

  ext_chan1 <- channel
  ext_chan2 <- channel

  externalMonitor "extMon" $ do

    handler c1out "send_ext" $ do
      e <- emitter (fst ext_chan1) 1
      callback $ \msg -> emit e msg

    handler (snd ext_chan2) "rcv_ext" $ do
      e <- emitter chtx 1
      callback $ \msg -> emit e msg



      
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
