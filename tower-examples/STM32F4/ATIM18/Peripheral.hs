{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
--
-- Peripheral.hs --- Advanced Timer (TIM1 to TIM8) Peripheral driver.
-- Defines peripheral types, instances.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module ATIM18.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import ATIM18.Regs
import RCC
import MemoryMap

-- Convenience type synonyms
data ATIM = ATIM
  { atimRegCR1          :: BitDataReg ATIM_CR1
  , atimRegCR2          :: BitDataReg ATIM_CR2
  , atimRegSMCR         :: BitDataReg ATIM_SMCR
  , atimRegDIER         :: BitDataReg ATIM_DIER
  , atimRegSR           :: BitDataReg ATIM_SR
  , atimRegEGR          :: BitDataReg ATIM_EGR
  , atimRegCCMR1_OCM    :: BitDataReg ATIM_CCMR1_OCM
  , atimRegCCMR1_ICM    :: BitDataReg ATIM_CCMR1_ICM
  , atimRegCCMR2_OCM    :: BitDataReg ATIM_CCMR2_OCM
  , atimRegCCMR2_ICM    :: BitDataReg ATIM_CCMR2_ICM
  , atimRegCCER         :: BitDataReg ATIM_CCER
  , atimRegCNT          :: BitDataReg ATIM_16
  , atimRegPSC          :: BitDataReg ATIM_PSC
  , atimRegARR          :: BitDataReg ATIM_16
  , atimRegCCR1         :: BitDataReg ATIM_16
  , atimRegCCR2         :: BitDataReg ATIM_16
  , atimRegCCR3         :: BitDataReg ATIM_16
  , atimRegCCR4         :: BitDataReg ATIM_16
  , atimRegBDTR         :: BitDataReg ATIM_BDTR
  , atimRCCEnable       :: forall eff . Ivory eff ()
  , atimRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create an ATIM given the base register address.
mkATIM :: Integer -> BitDataField RCC_APB2ENR Bit -> ATIM
mkATIM base rccfield =
  ATIM
    { atimRegCR1         = mkBitDataReg $ base + 0x00
    , atimRegCR2         = mkBitDataReg $ base + 0x04
    , atimRegSMCR        = mkBitDataReg $ base + 0x08
    , atimRegDIER        = mkBitDataReg $ base + 0x0C
    , atimRegSR          = mkBitDataReg $ base + 0x10
    , atimRegEGR         = mkBitDataReg $ base + 0x14
    , atimRegCCMR1_OCM   = mkBitDataReg $ base + 0x18 -- aliased with icm
    , atimRegCCMR1_ICM   = mkBitDataReg $ base + 0x18
    , atimRegCCMR2_OCM   = mkBitDataReg $ base + 0x1C -- aliased with icm
    , atimRegCCMR2_ICM   = mkBitDataReg $ base + 0x1C
    , atimRegCCER        = mkBitDataReg $ base + 0x20
    , atimRegCNT         = mkBitDataReg $ base + 0x24
    , atimRegPSC         = mkBitDataReg $ base + 0x28
    , atimRegARR         = mkBitDataReg $ base + 0x2C
    , atimRegCCR1        = mkBitDataReg $ base + 0x34
    , atimRegCCR2        = mkBitDataReg $ base + 0x38
    , atimRegCCR3        = mkBitDataReg $ base + 0x3C
    , atimRegCCR4        = mkBitDataReg $ base + 0x40
    , atimRegBDTR        = mkBitDataReg $ base + 0x44
    , atimRCCEnable      = rccEnable  rccreg rccfield
    , atimRCCDisable     = rccDisable rccreg rccfield
    }
  where rccreg = regRCC_APB2ENR -- TIM1 and TIM8 are in APB2

tim1 :: ATIM
tim1 = mkATIM tim1_periph_base rcc_apb2en_tim1

tim8 :: ATIM
tim8 = mkATIM tim8_periph_base rcc_apb2en_tim8

