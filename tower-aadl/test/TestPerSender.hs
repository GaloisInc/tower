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
-- Call from a periodic thread.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import Ivory.Tower
import Ivory.Language
import Tower.AADL

test0 :: Tower e ()
test0 = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  (cin, _) <- channel
  per <- period (100`ms`)

  monitor "sender" $ do
    handler per "tick" $ do
      e <- emitter cin 1
      callback $ \msg -> do
        m <- deref msg
        call_ printf "Sender ping received %d. Writing to receiver.\n" m
        emitV e (m+1)

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main =
  runCompileAADL
    initialOpts { genDirOpts = Just "test-per"
                }
    test0

--------------------------------------------------------------------------------
-- Helpers

printf :: Def('[IString, ITime] :-> ())
printf = importProc "printf" "stdio.h"

towerDepModule :: Module
towerDepModule = package "towerDeps" (incl printf)
