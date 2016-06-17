{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.FilePath

import Ivory.Artifact
import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Config

import Tower.Mini

integratedTower :: Tower e ()
integratedTower = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  towerModule  fooModule
  towerDepends fooModule

  towerModule  driverModule
  towerDepends driverModule

  let cfile f = artifactFile f (pure $ "test" </> "cfiles" </> f)
  towerArtifact $ Incl $ cfile "intermon1.h"
  towerArtifact $ Src  $ cfile "intermon1.c"
  towerArtifact $ Incl $ cfile "intermon2.h"
  towerArtifact $ Src  $ cfile "intermon2.c"

  (_, extChanInOut) <- channel
  (chanInIn, chanInOut) <- channel

  externalMonitor "intermon1" $
    handler (extChanInOut :: ChanOutput ('Stored Uint8)) "get_data" $ do
      e <- emitter chanInIn 1
      callback $ \msg -> emit e msg

  (chanOutIn, chanOutOut) <- channel

  externalMonitor "intermon2" $
    handler chanOutOut "put_data" $
      callback $ \msg -> do
        call_ intermon2_put_data msg

  (internalChanIn, internalChanOut) <- channel

  monitor "bar" $ do
    handler chanInOut "handle_bar" $ do
      e <- emitter internalChanIn 1
      callback $ \byte -> do
        byte' <- deref byte
        call_ printf_byte "bar: %u\n" byte'
        let b = (byte' .% 2 ==? 0) ? (true, false)
        emitV e b

  monitor "quux" $ do
    handler internalChanOut "handle_quux" $ do
      e <- emitter chanOutIn 1
      callback $ \b -> do
        b' <- deref b
        call_ printf_bool "quux: %u\n" b'
        emitV e b'

intermon1_get_data :: Def('[Ref s ('Stored Uint8)] ':-> IBool)
intermon1_get_data = importProc "intermon1_get_data" "intermon1.h"

intermon1_put_data :: Def('[ConstRef s ('Stored Uint8)] ':-> ())
intermon1_put_data = importProc "intermon1_put_data" "intermon1.h"

intermon2_get_data :: Def('[Ref s ('Stored IBool)] ':-> IBool)
intermon2_get_data = importProc "intermon2_get_data" "intermon2.h"

intermon2_put_data :: Def('[ConstRef s ('Stored IBool)] ':-> ())
intermon2_put_data = importProc "intermon2_put_data" "intermon2.h"

intermon1_callback :: Def('[ConstRef s ('Stored Uint8)] ':-> ())
intermon1_callback = importProc "callback_get_data" "intermon1_monitor.h"

fooModule :: Module
fooModule = package "foo" $ do
  incl intermon1_get_data
  incl intermon2_put_data
  dependByName "intermon1_monitor"
  incl run

run :: Def ('[] ':-> ())
run = voidProc "run" $ body $ do
  intermon1_data <- local izero
  intermon1_has_data <- call intermon1_get_data intermon1_data
  ifte_ intermon1_has_data
    (call_ intermon1_callback $ constRef intermon1_data)
    (return ())
  retVoid


driverModule :: Module
driverModule = package "driver" $ do
  incl intermon1_put_data
  incl intermon2_get_data
  incl printf_byte
  incl printf_bool
  depend fooModule
  let ivoryMain :: Def('[] ':-> Sint32)
      ivoryMain = proc "main" $ body $ do
        upTo (0 :: Ix 11) 10 $ \ix -> do
          byteRef <- local (ival (castDefault (fromIx ix)))
          byte <- deref byteRef
          call_ printf_byte "driver in: %u\n" byte
          call_ intermon1_put_data (constRef byteRef)
          call_ run
          bRef <- local izero
          has_data <- call intermon2_get_data bRef
          ifte_ has_data
            (do b <- deref bRef; call_ printf_bool "driver out: %u\n" b)
            (call_ printf_bool "driver didn't get data back!\n" false)
        ret 0
  incl ivoryMain

driverModule' :: Module
driverModule' = package "driver" $ do
  incl intermon1_put_data
  incl intermon2_get_data
  incl printf_byte
  incl printf_bool
  dependByName "foo"
  let ivoryMain :: Def('[] ':-> Sint32)
      ivoryMain = proc "main" $ body $ do
        upTo (0 :: Ix 11) 10 $ \ix -> do
          byteRef <- local (ival (castDefault (fromIx ix)))
          byte <- deref byteRef
          call_ printf_byte "driver in: %u\n" byte
          call_ intermon1_put_data (constRef byteRef)
          zero <- constRef `fmap` local izero
          call_ (importProc "component_entry" "foo.h" :: Def('[ConstRef s ('Stored Sint64)] ':-> ())) zero
          bRef <- local izero
          has_data <- call intermon2_get_data bRef
          ifte_ has_data
            (do b <- deref bRef; call_ printf_bool "driver out: %u\n" b)
            (call_ printf_bool "driver didn't get data back!\n" false)
        ret 0
  incl ivoryMain


integratedTower' :: Component e
integratedTower' = component "foo" $ do
  chanIn  <- inputPort  "intermon1_get_data" "intermon1.h"
  chanOut <- outputPort "intermon2_put_data" "intermon2.h"
  tower $ do
    towerModule  towerDepModule
    towerDepends towerDepModule

    towerModule  driverModule'
    towerDepends driverModule'

    let cfile f = artifactFile f (pure $ "test" </> "cfiles" </> f)
    towerArtifact $ Incl $ cfile "intermon1.h"
    towerArtifact $ Src  $ cfile "intermon1.c"
    towerArtifact $ Incl $ cfile "intermon2.h"
    towerArtifact $ Src  $ cfile "intermon2.c"

    (internalChanIn, internalChanOut) <- channel

    monitor "bar" $ do
      handler chanIn "handle_bar" $ do
        e <- emitter internalChanIn 1
        callback $ \byte -> do
          byte' <- deref byte
          call_ printf_byte "bar: %u\n" byte'
          let b = (byte' .% 2 ==? 0) ? (true, false)
          emitV e b

    monitor "quux" $ do
      handler internalChanOut "handle_quux" $ do
        e <- emitter chanOut 1
        callback $ \b -> do
          b' <- deref b
          call_ printf_bool "quux: %u\n" b'
          emitV e b'

main :: IO ()
main = compileTowerMini id p [integratedTower']
  where
  p topts = getConfig topts $ miniConfigParser defaultMiniConfig

--------------------------------------------------------------------------------
[ivory|
import (stdio.h, printf) void printf_byte(string x, uint8_t y)
import (stdio.h, printf) void printf_bool(string x, bool y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf_byte
  incl printf_bool
