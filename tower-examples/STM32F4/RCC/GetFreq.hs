{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- GetFreq.hs --- Run-time frequency information from RCC driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module RCC.GetFreq where

import Ivory.Language
import Ivory.Stdlib
import Ivory.BitData
import Ivory.HW

import RCC.RegTypes
import RCC.Regs

class BoardHSE p where
  hseFreq :: Proxy p -> Uint32

eqBits :: (BitData a) => a -> a -> IBool
eqBits l r = (toBits l) ==? (toBits r)

hsiFreq :: Uint32
hsiFreq = 16000000 -- from stm32f4xx.h

getFreqSysClk :: (GetAlloc eff ~ Scope s, BoardHSE p)
              => Proxy p -> Ivory eff Uint32
getFreqSysClk p = do
  cfgr <- getReg regRCC_CFGR
  sysClkSource p (cfgr #. rcc_cfgr_sws)

-- Catchall is hsiFreq, should be impossible.
sysClkSource :: (BoardHSE p, GetAlloc eff ~ Scope s)
             => Proxy p -> RCC_SYSCLK -> Ivory eff Uint32
sysClkSource p sws = foldl aux (return hsiFreq) tbl
  where aux k (v,d) = ifte (eqBits sws v) d k
        tbl = [(rcc_sysclk_hsi, return hsiFreq)
              ,(rcc_sysclk_hse, return (hseFreq p))
              ,(rcc_sysclk_pll, pllSysClk p)
              ]

pllSysClk :: (BoardHSE p) => Proxy p -> Ivory eff Uint32
pllSysClk p = do
  pllcfgr <- getReg regRCC_PLLCFGR
  let pllm    = safeCast $ toRep $ pllcfgr #. rcc_pllcfgr_pllm
      plln    = safeCast $ toRep $ pllcfgr #. rcc_pllcfgr_plln
      srcFreq = (toRep (pllcfgr #. rcc_pllcfgr_pllsrc) >? 0) ? (hseFreq p,hsiFreq)
      pllvco  = (srcFreq `iDiv` pllm) * plln
      pllp    = pllpToInt $ pllcfgr #. rcc_pllcfgr_pllp
  return (pllvco `iDiv` pllp)
  where
  pllpToInt p' = foldl aux 1 tbl -- Catchall is 1, should be impossible.
    where
    aux k (v,d) = (eqBits p' v) ? (d,k)
    tbl = [(rcc_pllp_div2,   2)
          ,(rcc_pllp_div4,   4)
          ,(rcc_pllp_div6,   6)
          ,(rcc_pllp_div8,   8)
          ]

getFreqHClk :: (GetAlloc eff ~ Scope s, BoardHSE p)
            => Proxy p -> Ivory eff Uint32
getFreqHClk p = do
  sysclk <- getFreqSysClk p
  cfgr <- getReg regRCC_CFGR
  return $ divideHPRE (cfgr #. rcc_cfgr_hpre) sysclk

getFreqPClk1 :: (GetAlloc eff ~ Scope s, BoardHSE p)
             => Proxy p -> Ivory eff Uint32
getFreqPClk1 p = do
  sysclk <- getFreqSysClk p
  cfgr <- getReg regRCC_CFGR
  dividePPREx (cfgr #. rcc_cfgr_ppre1) sysclk

getFreqPClk2 :: (GetAlloc eff ~ Scope s, BoardHSE p)
             => Proxy p -> Ivory eff Uint32
getFreqPClk2 p = do
  sysclk <- getFreqSysClk p
  cfgr <- getReg regRCC_CFGR
  dividePPREx (cfgr #. rcc_cfgr_ppre2) sysclk

data PClk = PClk1 | PClk2

getFreqPClk :: (GetAlloc eff ~ Scope s, BoardHSE p) => Proxy p -> PClk -> Ivory eff Uint32
getFreqPClk p PClk1 = getFreqPClk1 p
getFreqPClk p PClk2 = getFreqPClk2 p

divideHPRE :: RCC_HPRE -> Uint32 -> Uint32
divideHPRE hpre n = n `iDiv` divisor
  where
  divisor = foldl aux 1 tbl -- Catchall is 1: none has bits 0b0xxx
  aux k (hpreV, d) = (eqBits hpreV hpre) ? (d,k)
  tbl = [(rcc_hpre_none,   1)
        ,(rcc_hpre_div2,   2)
        ,(rcc_hpre_div4,   4)
        ,(rcc_hpre_div8,   8)
        ,(rcc_hpre_div16,  16)
        ,(rcc_hpre_div64,  64)
        ,(rcc_hpre_div128, 128)
        ,(rcc_hpre_div256, 256)
        ,(rcc_hpre_div512, 512)
        ]

dividePPREx :: RCC_PPREx -> Uint32 -> Ivory eff Uint32
dividePPREx pprex n = do
  d <- assign divisor
  return (n `iDiv` d)
  where
  divisor = foldl aux 1 tbl -- Catchall is 1: none has bits 0b0xx
  aux k (ppreV, d) = (eqBits ppreV pprex) ? (d,k)
  tbl = [(rcc_pprex_none,   1)
        ,(rcc_pprex_div2,   2)
        ,(rcc_pprex_div4,   4)
        ,(rcc_pprex_div8,   8)
        ,(rcc_pprex_div16,  16)
        ]

