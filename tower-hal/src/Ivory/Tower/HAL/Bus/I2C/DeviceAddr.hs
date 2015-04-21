{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.HAL.Bus.I2C.DeviceAddr
  ( I2CDeviceAddr(..)
  , readAddr
  , writeAddr
  ) where

import Ivory.Language

newtype I2CDeviceAddr = I2CDeviceAddr Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryStore, IvoryInit, IvoryZeroVal)

readAddr :: I2CDeviceAddr -> Uint8
readAddr (I2CDeviceAddr a) = 2 * a + 1
writeAddr :: I2CDeviceAddr -> Uint8
writeAddr (I2CDeviceAddr a) = 2 * a
