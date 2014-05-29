{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
--
-- Class.hs --- Type class for RCC devices.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module RCC.Class where

import Ivory.Language
import Ivory.HW

class RCCDevice a where
  rccDeviceEnable  :: a -> Ivory eff ()
  rccDeviceDisable :: a -> Ivory eff ()

--class (BitData (RCCEnableReg a),
--       IvoryIOReg (BitDataRep (RCCEnableReg a)))
--    => RCCDevice a where
--  type RCCEnableReg a
--  rccDeviceEnableReg   :: a -> BitDataReg (RCCEnableReg a)
--  rccDeviceEnableField :: a -> BitDataField (RCCEnableReg a) Bit

rccEnable :: (BitData a, IvoryIOReg (BitDataRep a))
          => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccEnable reg field = modifyReg reg (setBit field)

rccDisable :: (BitData a, IvoryIOReg (BitDataRep a))
           => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccDisable reg field = modifyReg reg (clearBit field)

