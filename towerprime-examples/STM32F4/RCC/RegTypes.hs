{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--
-- RegTypes.hs --- Types for register fields in RCC driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module RCC.RegTypes where

import Ivory.BitData

[bitdata|
 bitdata RCC_PLLP     :: Bits 2
 = rcc_pllp_div2      as 0
 | rcc_pllp_div4      as 1
 | rcc_pllp_div6      as 2
 | rcc_pllp_div8      as 3

 bitdata RCC_MCOx     :: Bits 2
 = rcc_mcox_sysclk    as 0
 | rcc_mcox_plli2s    as 1
 | rcc_mcox_hse       as 2
 | rcc_mcox_pll       as 3

 bitdata RCC_MCOxPre  :: Bits 3
  = rcc_mcoxpre_none  as 0 -- 0b0xx
  | rcc_mcoxpre_div2  as 4 -- 0b100
  | rcc_mcoxpre_div3  as 5 -- 0b101
  | rcc_mcoxpre_div4  as 6 -- 0b110
  | rcc_mcoxpre_div5  as 7 -- 0b111

 bitdata RCC_PPREx    :: Bits 3
  = rcc_pprex_none    as 0 -- 0b0xx
  | rcc_pprex_div2    as 4 -- 0b100
  | rcc_pprex_div4    as 5 -- 0b100
  | rcc_pprex_div8    as 6 -- 0b100
  | rcc_pprex_div16   as 7 -- 0b111

 bitdata RCC_HPRE     :: Bits 4
  = rcc_hpre_none     as 0  -- 0b0xxx
  | rcc_hpre_div2     as 8  -- 0b1000
  | rcc_hpre_div4     as 9  -- 0b1001
  | rcc_hpre_div8     as 10 -- 0b1010
  | rcc_hpre_div16    as 11 -- 0b1011
  | rcc_hpre_div64    as 12 -- 0b1100
  | rcc_hpre_div128   as 13 -- 0b1101
  | rcc_hpre_div256   as 14 -- 0b1110
  | rcc_hpre_div512   as 15 -- 0b1111

 bitdata RCC_SYSCLK   :: Bits 2
  = rcc_sysclk_hsi    as 0
  | rcc_sysclk_hse    as 1
  | rcc_sysclk_pll    as 2

|]
