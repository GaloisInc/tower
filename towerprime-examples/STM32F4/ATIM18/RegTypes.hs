{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- Advanced Timer (TIM1 and TIM8)  register types
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module ATIM18.RegTypes where

import Ivory.BitData

-- Compare Mode bit field definitions:
[bitdata|
 bitdata CCMRMode :: Bits 3
   = ccmr_mode_frzn     as 0
   | ccmr_mode_chact    as 1
   | ccmr_mode_chinact  as 2
   | ccmr_mode_ocreftog as 3
   | ccmr_mode_ocreflo  as 4
   | ccmr_mode_ocrefhi  as 5
   | ccmr_mode_pwm1     as 6
   | ccmr_mode_pwm2     as 7
|]

-- Capture/Compare Selection bit field definitions:
[bitdata|
 bitdata CCSMode :: Bits 2
   = ccs_mode_out   as 0
   | ccs_mode_in1   as 1
   | ccs_mode_in2   as 2
   | ccs_mode_intrc as 3
|]

