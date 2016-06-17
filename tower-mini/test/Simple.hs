{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Config

import Tower.Mini

simpleTower :: Component e
simpleTower = component "simple" $ tower $ do
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

main :: IO ()
main = compileTowerMini id p [simpleTower]
  where
  p topts = getConfig topts $ miniConfigParser defaultMiniConfig

--------------------------------------------------------------------------------
[ivory|
import (stdio.h, printf) void printf(string x, uint8_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf
