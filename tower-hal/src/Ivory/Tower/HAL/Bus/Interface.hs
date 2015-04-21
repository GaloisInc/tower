{-# LANGUAGE DataKinds #-}

module Ivory.Tower.HAL.Bus.Interface where

import Ivory.Language
import Ivory.Tower

data BackpressureTransmit value status = BackpressureTransmit
  { backpressureTransmit :: ChanInput value
  , backpressureComplete :: ChanOutput status
  }

data AbortableTransmit value status = AbortableTransmit
  { abortableTransmit :: ChanInput value
  , abortableAbort :: ChanInput (Stored IBool)
  , abortableComplete :: ChanOutput status
  }
