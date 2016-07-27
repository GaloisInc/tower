{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Bus.CAN where

import Ivory.Language

-- CAN arbitration rules:
-- * Higher priorities have lower numeric values.
-- * Convert standard-frame 11-bit IDs to 29 bits by
--   choosing the highest-priority 29-bit ID that has the
--   same most-significant 11 bits as the original ID. In
--   other words, pad with 18 zeroes on the right.
-- * Standard frames are higher priority than extended
--   frames with the same 29-bit ID, so append a 1 for
--   extended frames or a 0 for standard frames.
-- * Remote frames are lower priority than data frames of
--   the same ID, so append a 1 for remote frames or a 0 for
--   data frames.

[ivory|
bitdata CANArbitrationField :: Bits 32 = can_arbitration_field
  { _                   :: Bit
  , can_arbitration_id  :: Bits 29
  , can_arbitration_ide :: Bit
  , can_arbitration_rtr :: Bit
  }

struct can_message
  { can_message_id  :: Stored CANArbitrationField
  ; can_message_len :: Stored (Ix 9)
  ; can_message_buf :: Array 8 (Stored Uint8)
  }
|]

standardCANID :: Bits 11 -> Bit -> CANArbitrationField
standardCANID stdid rtr = fromRep $ withBits 0 $ do
  setField can_arbitration_id $ fromRep $ safeCast (toRep stdid) `iShiftL` 18
  setField can_arbitration_rtr rtr

extendedCANID :: Bits 29 -> Bit -> CANArbitrationField
extendedCANID extid rtr = fromRep $ withBits 0 $ do
  setField can_arbitration_id extid
  setBit can_arbitration_ide
  setField can_arbitration_rtr rtr

canDriverTypes :: Module
canDriverTypes = package "canDriverTypes" $ do
  defStruct (Proxy :: Proxy "can_message")
