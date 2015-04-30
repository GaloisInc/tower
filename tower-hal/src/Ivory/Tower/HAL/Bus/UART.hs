{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Bus.UART where

import Ivory.Language

[ivory|
struct uart_transaction_request
  { tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored Sint32  -- not (Ix 128) because of fencepost bugs
  }
|]

uartDriverTypes :: Module
uartDriverTypes = package "uartDriverTypes" $ do
  defStruct (Proxy :: Proxy "uart_transaction_request")

