--
-- RCC.hs --- RCC (Reset and Clock Control) peripheral driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module RCC (
    RCCDevice(..), rccEnable, rccDisable
  , module RCC.Regs

  -- * system clock frequency
  , PClk(..)
  , BoardHSE(..)
  , getFreqSysClk
  , getFreqHClk
  , getFreqPClk1
  , getFreqPClk2
  , getFreqPClk
) where

import RCC.Class
import RCC.Regs
import RCC.GetFreq
