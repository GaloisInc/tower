{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.GRTOS.Atomic
  ( enter
  , exit
  , moddef
  ) where

import Ivory.Language

enter :: Def ('[] :-> ())
enter = importProc "ivory_grtos_begin_atomic" atomicWrapperHeader

exit :: Def ('[] :-> ())
exit = importProc "ivory_grtos_end_atomic" atomicWrapperHeader

atomicWrapperHeader :: String
atomicWrapperHeader = "grtos_atomic_wrapper.h"

moddef :: ModuleDef
moddef = do
  inclHeader atomicWrapperHeader
  sourceDep atomicWrapperHeader
  sourceDep "grtos_atomic_wrapper.c"

