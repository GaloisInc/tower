{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Atomic where

import Ivory.Language

atomicHeader :: String
atomicHeader = "freertos_atomic_wrapper.h"

begin_atomic :: Def ('[]:->())
begin_atomic = importProc "ivory_freertos_begin_atomic" atomicHeader

end_atomic :: Def ('[]:->())
end_atomic = importProc "ivory_freertos_end_atomic" atomicHeader

atomic_block :: (NoReturn ~ GetReturn eff) => Ivory eff () -> Ivory eff ()
atomic_block k = do
  call_ begin_atomic
  k
  call_ end_atomic

