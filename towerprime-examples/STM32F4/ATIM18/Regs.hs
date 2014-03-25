{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- ATIM18/Regs.hs --- Advanced Timer (TIM1 and TIM8) registers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module ATIM18.Regs where

import Ivory.BitData
import ATIM18.RegTypes

[bitdata|
 bitdata ATIM_CR1 :: Bits 32 = atim_cr1
  { _             :: Bits 22
  , atim_cr1_ckd  :: Bits 2
  , atim_cr1_arpe :: Bit
  , atim_cr1_cms  :: Bits 2
  , atim_cr1_dir  :: Bit
  , atim_cr1_opm  :: Bit
  , atim_cr1_urs  :: Bit
  , atim_cr1_udis :: Bit
  , atim_cr1_cen  :: Bit
  }
|]

[bitdata|
 bitdata ATIM_CR2 :: Bits 32 = atim_cr2 -- XXX FIXME LATER
  { _             :: Bits 32
  }
|]

[bitdata|
 bitdata ATIM_SMCR  :: Bits 32 = atim_smcr
  { _               :: Bits 16
  , atim_smcr_etp   :: Bit
  , atim_smcr_ece   :: Bit
  , atim_smcr_etps  :: Bits 2
  , atim_smcr_etf   :: Bits 4
  , atim_smcr_msm   :: Bit
  , atim_smcr_ts    :: Bits 3
  , _               :: Bit
  , atim_smcr_sms   :: Bits 3
  }
|]

[bitdata|
 bitdata ATIM_DIER  :: Bits 32 = atim_dier
  { _               :: Bits 17
  , atim_dier_tde   :: Bit
  , atim_dier_comde :: Bit
  , atim_dier_cc4de :: Bit
  , atim_dier_cc3de :: Bit
  , atim_dier_cc2de :: Bit
  , atim_dier_cc1de :: Bit
  , atim_dier_ude   :: Bit
  , atim_dier_bde   :: Bit
  , atim_dier_tie   :: Bit
  , atim_dier_comie :: Bit
  , atim_dier_cc4ie :: Bit
  , atim_dier_cc3ie :: Bit
  , atim_dier_cc2ie :: Bit
  , atim_dier_cc1ie :: Bit
  , atim_dier_uie   :: Bit
  }
|]

[bitdata|
 bitdata ATIM_SR    :: Bits 32 = atim_sr
  { _               :: Bits 19
  , atim_sr_cc4of   :: Bit
  , atim_sr_cc3of   :: Bit
  , atim_sr_cc2of   :: Bit
  , atim_sr_cc1of   :: Bit
  , _               :: Bit
  , atim_sr_bif     :: Bit
  , atim_sr_tif     :: Bit
  , atim_sr_comif   :: Bit
  , atim_sr_cc4if   :: Bit
  , atim_sr_cc3if   :: Bit
  , atim_sr_cc2if   :: Bit
  , atim_sr_cc1if   :: Bit
  , atim_sr_uif     :: Bit
  }
|]

[bitdata|
 bitdata ATIM_EGR   :: Bits 32 = atim_egr
  { _               :: Bits 24
  , atim_egr_bg     :: Bit
  , atim_egr_tg     :: Bit
  , atim_egr_comg   :: Bit
  , atim_egr_cc4g   :: Bit
  , atim_egr_cc3g   :: Bit
  , atim_egr_cc2g   :: Bit
  , atim_egr_cc1g   :: Bit
  , atim_egr_ug     :: Bit
  }
|]

[bitdata|
 bitdata ATIM_CCMR1_OCM    :: Bits 32 = atim_ccmr1_ocm
  { _                      :: Bits 16
  , atim_ccmr1_ocm_oc2ce   :: Bit
  , atim_ccmr1_ocm_oc2m    :: CCMRMode
  , atim_ccmr1_ocm_oc2pe   :: Bit
  , atim_ccmr1_ocm_oc2fe   :: Bit
  , atim_ccmr1_ocm_cc2s    :: CCSMode
  , atim_ccmr1_ocm_oc1ce   :: Bit
  , atim_ccmr1_ocm_oc1m    :: CCMRMode
  , atim_ccmr1_ocm_oc1pe   :: Bit
  , atim_ccmr1_ocm_oc1fe   :: Bit
  , atim_ccmr1_ocm_cc1s    :: CCSMode
  }
|]

[bitdata|
 bitdata ATIM_CCMR1_ICM    :: Bits 32 = atim_ccmr1_icm
  { _                      :: Bits 16
  , atim_ccmr1_icm_ic2f    :: Bits 4
  , atim_ccmr1_icm_ic2psc  :: Bits 2
  , atim_ccmr1_icm_cc2s    :: CCSMode
  , atim_ccmr1_icm_ic1f    :: Bits 4
  , atim_ccmr1_icm_ic1psc  :: Bits 2
  , atim_ccmr1_icm_cc1s    :: CCSMode
  }
|]

[bitdata|
 bitdata ATIM_CCMR2_OCM    :: Bits 32 = atim_ccmr2_ocm
  { _                      :: Bits 16
  , atim_ccmr2_ocm_oc4ce   :: Bit
  , atim_ccmr2_ocm_oc4m    :: CCMRMode
  , atim_ccmr2_ocm_oc4pe   :: Bit
  , atim_ccmr2_ocm_oc4fe   :: Bit
  , atim_ccmr2_ocm_cc4s    :: CCSMode
  , atim_ccmr2_ocm_oc3ce   :: Bit
  , atim_ccmr2_ocm_oc3m    :: CCMRMode
  , atim_ccmr2_ocm_oc3pe   :: Bit
  , atim_ccmr2_ocm_oc3fe   :: Bit
  , atim_ccmr2_ocm_cc3s    :: CCSMode
  }
|]

[bitdata|
 bitdata ATIM_CCMR2_ICM    :: Bits 32 = atim_ccmr2_icm
  { _                      :: Bits 16
  , atim_ccmr2_icm_ic4f    :: Bits 4
  , atim_ccmr2_icm_ic4psc  :: Bits 2
  , atim_ccmr2_icm_cc4s    :: CCSMode
  , atim_ccmr2_icm_ic3f    :: Bits 4
  , atim_ccmr2_icm_ic3psc  :: Bits 2
  , atim_ccmr2_icm_cc3s    :: CCSMode
  }
|]

[bitdata|
 bitdata ATIM_CCER         :: Bits 32 = atim_ccer
  { _                      :: Bits 18
  , atim_ccer_cc4p         :: Bit
  , atim_ccer_cc4e         :: Bit
  , atim_ccer_cc3np        :: Bit
  , atim_ccer_ccne         :: Bit
  , atim_ccer_cc3p         :: Bit
  , atim_ccer_cc3e         :: Bit
  , atim_ccer_cc2np        :: Bit
  , atim_ccer_cc2ne        :: Bit
  , atim_ccer_cc2p         :: Bit
  , atim_ccer_cc2e         :: Bit
  , atim_ccer_cc1np        :: Bit
  , atim_ccer_cc1ne        :: Bit
  , atim_ccer_cc1p         :: Bit
  , atim_ccer_cc1e         :: Bit
  }
|]

[bitdata|
 bitdata ATIM_BDTR :: Bits 32 = atim_bdtr -- XXX FIXME LATER
  { _             :: Bits 32
  }
|]

[bitdata|
 bitdata ATIM_PSC          :: Bits 32 = atim_psc
  { _                      :: Bits 16
  , atim_psc_psc           :: Bits 16
  }
|]

[bitdata|
 bitdata ATIM_16           :: Bits 32 = atim_16
  { _                      :: Bits 16
  , atim_16_data           :: Bits 16
  }
|]
