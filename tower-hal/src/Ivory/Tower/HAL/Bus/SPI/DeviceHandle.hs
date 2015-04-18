{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.HAL.Bus.SPI.DeviceHandle
  ( SPIDeviceHandle(..)
  ) where

import Ivory.Language

newtype SPIDeviceHandle = SPIDeviceHandle Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryStore, IvoryInit, IvoryZeroVal)
