{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- RCC (Reset and Clock Control) registers.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module RCC.Regs where

import Ivory.Language
import Ivory.HW

import MemoryMap (rcc_periph_base)

import RCC.RegTypes

-- This driver is, for now, incomplete: it only covers the config register and
-- peripheral clock enable registers, and leaves out the various other system
-- clock, peripheral reset, and peripheral clock low power mode registers which
-- we have no use for at the moment.


-- PLL Configuration Register --------------------------------------------------

[ivory|
 bitdata RCC_PLLCFGR :: Bits 32 = rcc_pllcfgr
  { rcc_pllcfgr_pllq      :: Bits 4 --  2 <= pllq <= 15
  , _                     :: Bit
  , rcc_pllcfgr_pllsrc    :: Bit
  , _                     :: Bits 4
  , rcc_pllcfgr_pllp      :: RCC_PLLP
  , _                     :: Bit
  , rcc_pllcfgr_plln      :: Bits 9 -- 192 <= plln <= 432
  , rcc_pllcfgr_pllm      :: Bits 6 --   2 <= pllm <= 63
  }
|]

regRCC_PLLCFGR :: BitDataReg RCC_PLLCFGR
regRCC_PLLCFGR = mkBitDataReg $ rcc_periph_base + 0x04

-- Clock Configuration Register ------------------------------------------------

[ivory|
 bitdata RCC_CFGR :: Bits 32 = rcc_cfgr
  { rcc_cfgr_mco2         :: RCC_MCOx
  , rcc_cfgr_mco2_pre     :: RCC_MCOxPre
  , rcc_cfgr_mco1_pre     :: RCC_MCOxPre
  , rcc_cfgr_i2ssrc       :: Bit
  , rcc_cfgr_mco1         :: RCC_MCOx
  , rcc_cfgr_rtcpre       :: Bits 5
  , rcc_cfgr_ppre2        :: RCC_PPREx
  , rcc_cfgr_ppre1        :: RCC_PPREx
  , _                     :: Bits 2
  , rcc_cfgr_hpre         :: RCC_HPRE
  , rcc_cfgr_sws          :: RCC_SYSCLK
  , rcc_cfgr_sw           :: RCC_SYSCLK
  }
|]

regRCC_CFGR :: BitDataReg RCC_CFGR
regRCC_CFGR = mkBitDataReg $ rcc_periph_base + 0x08


-- AHB Peripheral Clock Enable Registers ---------------------------------------

[ivory|
 bitdata RCC_AHB1ENR :: Bits 32 = rcc_ahb1enr
  { _                     :: Bit
  , rcc_ahb1en_otg_hsulpi :: Bit
  , rcc_ahb1en_otg_hs     :: Bit
  , rcc_ahb1en_ethmactx   :: Bit
  , rcc_ahb1en_ethmacrx   :: Bit
  , rcc_ahb1en_ethmac     :: Bit
  , _                     :: Bits 2
  , rcc_ahb1en_dma2       :: Bit
  , rcc_ahb1en_dma1       :: Bit
  , rcc_ahb1en_ccmdataram :: Bit
  , _                     :: Bit
  , rcc_ahb1en_bkpsram    :: Bit
  , _                     :: Bits 5
  , rcc_ahb1en_crc        :: Bit
  , _                     :: Bits 3
  , rcc_ahb1en_gpioi      :: Bit
  , rcc_ahb1en_gpioh      :: Bit
  , rcc_ahb1en_gpiog      :: Bit
  , rcc_ahb1en_gpiof      :: Bit
  , rcc_ahb1en_gpioe      :: Bit
  , rcc_ahb1en_gpiod      :: Bit
  , rcc_ahb1en_gpioc      :: Bit
  , rcc_ahb1en_gpiob      :: Bit
  , rcc_ahb1en_gpioa      :: Bit
  }
|]

regRCC_AHB1ENR :: BitDataReg RCC_AHB1ENR
regRCC_AHB1ENR = mkBitDataReg $ rcc_periph_base + 0x30

[ivory|
 bitdata RCC_AHB2ENR :: Bits 32 = rcc_ahb2enr
  { _                     :: Bits 24
  , rcc_ahb2en_otg_fs     :: Bit
  , rcc_ahb2en_rng        :: Bit
  , rcc_ahb2en_hash       :: Bit
  , rcc_ahb2en_cryp       :: Bit
  , _                     :: Bits 3
  , rcc_ahb2en_dcmi       :: Bit
  }
|]

regRCC_AHB2ENR :: BitDataReg RCC_AHB2ENR
regRCC_AHB2ENR = mkBitDataReg $ rcc_periph_base + 0x34

[ivory|
 bitdata RCC_AHB3ENR :: Bits 32 = rcc_ahb3enr
  { _                     :: Bits 31
  , rcc_ahb3en_fsmc       :: Bit
  }
|]

regRCC_AHB3ENR :: BitDataReg RCC_AHB3ENR
regRCC_AHB3ENR = mkBitDataReg $ rcc_periph_base + 0x38

-- APB Peripheral Clock Enable Registers ---------------------------------------
-- Note: the reference manual lists separate register maps for two subsets of
-- the STM32F4, the F42xxx/F43xxx and the (F405xx/07xx and F415xx/17xx)
-- subfamlies. These types match the F42xxx/F43xxx series, which is mostly a
-- superset of the F405&c. series, with the exception that F405 has a SPI4 and
-- F42xxx does not.

[ivory|
 bitdata RCC_APB1ENR :: Bits 32 = rcc_apb1enr
  { rcc_apb1en_uart8      :: Bit
  , rcc_apb1en_uart7      :: Bit
  , rcc_apb1en_dac        :: Bit
  , rcc_apb1en_pwr        :: Bit
  , _                     :: Bit
  , rcc_apb1en_can2       :: Bit
  , rcc_apb1en_can1       :: Bit
  , _                     :: Bit
  , rcc_apb1en_i2c3       :: Bit
  , rcc_apb1en_i2c2       :: Bit
  , rcc_apb1en_i2c1       :: Bit
  , rcc_apb1en_uart5      :: Bit
  , rcc_apb1en_uart4      :: Bit
  , rcc_apb1en_uart3      :: Bit
  , rcc_apb1en_uart2      :: Bit
  , _                     :: Bit
  , rcc_apb1en_spi3       :: Bit
  , rcc_apb1en_spi2       :: Bit
  , _                     :: Bits 2
  , rcc_apb1en_wwdg       :: Bit
  , _                     :: Bits 2
  , rcc_apb1en_tim14      :: Bit
  , rcc_apb1en_tim13      :: Bit
  , rcc_apb1en_tim12      :: Bit
  , rcc_apb1en_tim7       :: Bit
  , rcc_apb1en_tim6       :: Bit
  , rcc_apb1en_tim5       :: Bit
  , rcc_apb1en_tim4       :: Bit
  , rcc_apb1en_tim3       :: Bit
  , rcc_apb1en_tim2       :: Bit
  }
|]

regRCC_APB1ENR :: BitDataReg RCC_APB1ENR
regRCC_APB1ENR = mkBitDataReg $ rcc_periph_base + 0x40

[ivory|
 bitdata RCC_APB2ENR :: Bits 32 = rcc_apb2enr
  { _                     :: Bits 13
  , rcc_apb2en_tim11      :: Bit
  , rcc_apb2en_tim10      :: Bit
  , rcc_apb2en_tim9       :: Bit
  , _                     :: Bit
  , rcc_apb2en_syscfg     :: Bit
  , _                     :: Bit
  , rcc_apb2en_spi1       :: Bit
  , rcc_apb2en_sdio       :: Bit
  , rcc_apb2en_adc3       :: Bit
  , rcc_apb2en_adc2       :: Bit
  , rcc_apb2en_adc1       :: Bit
  , _                     :: Bits 2
  , rcc_apb2en_uart6      :: Bit
  , rcc_apb2en_uart1      :: Bit
  , _                     :: Bits 2
  , rcc_apb2en_tim8       :: Bit
  , rcc_apb2en_tim1       :: Bit
  }
|]

regRCC_APB2ENR :: BitDataReg RCC_APB2ENR
regRCC_APB2ENR = mkBitDataReg $ rcc_periph_base + 0x44

