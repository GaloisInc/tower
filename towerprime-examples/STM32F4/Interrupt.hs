--
-- Types.hs -- Interrupt types for STM32F4
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Interrupt
  -- from Types:
  ( IRQ(..), irqn
  , Exception(..), exceptionIRQn, isrException
  , Interrupt(..), interruptIRQn, isrInterrupt
  -- from API:
  , interrupt_enable
  , interrupt_disable
  , interrupt_set_priority
  ) where

import Interrupt.Types
import Interrupt.API

