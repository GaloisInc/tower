--
-- API.hs --- Interrupt Controller API
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Interrupt.API where

import Interrupt.Types
import Interrupt.Regs

import Ivory.Language
import Ivory.BitData
import Ivory.HW

----------------------------------------------------------------------
-- High Level Interface

interrupt_enable :: Interrupt -> Ivory eff ()
interrupt_enable i = do
  let (reg, bitN) = nvic_ISER_int i
  setReg reg $ do
    setBit (nvic_iser_setena #> bitIx bitN)

interrupt_disable :: Interrupt -> Ivory eff ()
interrupt_disable i = do
  let (reg, bitN) = nvic_ICER_int i
  setReg reg $ do
    setBit (nvic_icer_clrena #> bitIx bitN)

-- | interrupt_set_priority: always give the priority as level 0 (highest) to 16
--   (lowest).
interrupt_set_priority :: Interrupt -> Uint8 -> Ivory eff ()
interrupt_set_priority i pri = do
  assert (pri <? (1 `iShiftL` nvic_prio_shift))
  let pri' = pri `iShiftL` nvic_prio_shift
  writeReg (nvic_IPR_int i) pri'
  where
  -- | The STM32F4 NVIC ignores writes to the low 4 bits of the
  -- interrupt priority registers.  We hide this from callers, so the
  -- API accepts interrupt priority levels from 0 to 15.
  nvic_prio_shift :: Uint8
  nvic_prio_shift = 4
