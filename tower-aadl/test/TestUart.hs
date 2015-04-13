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
-- UART client example
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import Ivory.Tower
import Ivory.Language
import Ivory.Stdlib
import Tower.AADL

--------------------------------------------------------------------------------
-- Types

[ivory|
struct uart_packet
  { uint8_t uart_num
  ; int64_t datum
  }
|]

data Uart = Uart
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

testUart :: Tower e ()
testUart = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  per <- period (100`ms`)

  -- From user code sender to wrapper
  (s2wTx, s2wRx) <- channel
  -- From wrapper to user code receiver
  (w2rTx, w2rRx) <- channel

  -- From wrapper to driver (unneeded to specify)
  (w2dTx, _ ) <- channel

  -- From driver to wrapper
  (_, d2wRx :: ChanOutput (Struct "uart_packet")) <- channel

  monitor "sender" $ do
    handler per "periodicHandler" $ do
      e <- emitter s2wTx 1 -- Send to wrapper
      callback $ \msg -> do
        m <- deref msg
        packet <- local izero
        call_ periodic_ping (toIMilliseconds m) packet
        emit e (constRef packet)

  monitor "receiver" $ do
    handler w2rRx "receiverHandler" $ do
      callback $ \msg -> do -- Receive from wrapper
        call_ ping_received msg

  externalMonitor "uart_monitor" $ do

    -- Rx from sender, define a symbol for handling msg, but don't implement it.
    handler s2wRx "send" $ do
      e <- emitter w2dTx 1
      callback $ \msg -> emit e msg

    -- From driver
    handler d2wRx "recv" $ do
      e <- emitter w2rTx 1
      callback $ \msg -> emit e msg

-- user_sender.c
periodic_ping :: Def('[Sint64, Ref s (Struct "uart_packet")] :-> ())
periodic_ping = proc "periodic_ping"
  $ \periodic_100_ms packet -> body $ do
  call_ printf "sender ping received (%lld).  Writing to receiver \n"
        periodic_100_ms
  store (packet ~> uart_num) 1
  store (packet ~> datum) (periodic_100_ms + 1)

-- user_receiver.c
ping_received :: Def('[ConstRef s (Struct "uart_packet")] :-> ())
ping_received = proc "ping_received" $ \input1 -> body $ do
  d <- input1 ~>* datum
  call_ printf "receiver ping received (%lld)\n" d

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main =
  runCompileAADL
    initialOpts { genDirOpts = Just "testUart"
                , configOpts = configOpts'
                }
    testUart
  where
  configOpts' = initialConfig { configSystemHW = "ODROID" }

--------------------------------------------------------------------------------
-- Helpers

[ivory|
-- import (smaccm_uart.h, send_handler)
--   void send_handler(const * struct uart_packet x)
-- import (smaccm_uart.h, recv_data) void recv_data(* struct uart_packet x)
import (stdio.h, printf) void printf(string x, int64_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  -- incl send_handler
  -- incl recv_data
  incl printf
  incl periodic_ping
  incl ping_received
  defStruct (Proxy :: (Proxy "uart_packet"))
