{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Bus.I2C where

import Ivory.Language
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr

[ivory|
struct i2c_transaction_request
  { tx_addr   :: Stored I2CDeviceAddr
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  ; rx_len    :: Stored (Ix 128)
  }

struct i2c_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  }

|]
