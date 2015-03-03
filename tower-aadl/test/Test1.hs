{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- Single call from an active to a passive thread.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import Ivory.Tower
import Ivory.Language
import Tower.AADL

test1 :: Tower e ()
test1 = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  (c1in, c1out) <- channel
  per <- period (100`ms`)

  monitor "sender" $ do
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \msg -> do
        m <- deref msg
        call_ printf "Sender ping received %d. Writing to receiver.\n" m
        emitV e (m+1)

  monitor "receiver1" $ do
    handler c1out "receiver1" $ do
      callback $ \msg -> do
        m <- deref msg
        call_ printf "receiver1 msg received %d.\n" m

    handler c1out "receiver2" $ do
      callback $ \msg -> do
        m <- deref msg
        call_ printf "receiver2 msg received %d.\n" m

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main =
  runCompileAADL
    initialOpts { genDirOpts = Just "test1/src"
                , configOpts = config
                }
    test1
  where
  config = initialConfig { configSrcsDir = "test1/user_code" }

--------------------------------------------------------------------------------
-- Helpers

printf :: Def('[IString, ITime] :-> ())
printf = importProc "printf" "stdio.h"

towerDepModule :: Module
towerDepModule = package "towerDeps" (incl printf)
