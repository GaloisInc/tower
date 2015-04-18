{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Bus.SPI where

import Ivory.Language
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

[ivory|
struct spi_transaction_request
  { tx_device :: Stored SPIDeviceHandle
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  }

struct spi_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  ; rx_idx     :: Stored (Ix 128)
  }

|]

spiDriverTypes :: Module
spiDriverTypes = package "spiDriverTypes" $ do
  defStruct (Proxy :: Proxy "spi_transaction_request")
  defStruct (Proxy :: Proxy "spi_transaction_result")
