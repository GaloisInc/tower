{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Atomic
  ( enter
  , exit
  , moddef
  ) where

import Ivory.Language

enter :: Def ('[] :-> ())
enter = importProc "ivory_freertos_begin_atomic" atomicWrapperHeader

exit :: Def ('[] :-> ())
exit = importProc "ivory_freertos_end_atomic" atomicWrapperHeader

atomicWrapperHeader :: String
atomicWrapperHeader = "freertos_atomic_wrapper.h"

moddef :: ModuleDef
moddef = do
  inclHeader atomicWrapperHeader
  sourceDep atomicWrapperHeader
  sourceDep "freertos_atomic_wrapper.c"

