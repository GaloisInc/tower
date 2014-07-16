{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.GRTOS.Time
  ( delay
  , delayUntil
  , getTickCount
  , getTickRateMilliseconds
  , moddef
  ) where

import Ivory.Language

moddef :: ModuleDef
moddef = do
  inclHeader timeWrapperHeader
  sourceDep timeWrapperHeader
  sourceDep "grtos_time_wrapper.c"

timeWrapperHeader :: String
timeWrapperHeader = "grtos_time_wrapper.h"

type Ticks = Uint32

delay :: Def ('[ Ticks ] :->())
delay = importProc "ivory_grtos_time_delay" timeWrapperHeader

delayUntil :: Def ('[ Ref s (Stored Ticks), Ticks ] :->())
delayUntil = importProc "ivory_grtos_time_delayuntil" timeWrapperHeader

getTickCount:: Def ('[] :-> Ticks)
getTickCount =
  importProc "ivory_grtos_time_gettickcount" timeWrapperHeader

getTickRateMilliseconds :: Def ('[] :-> Uint32)
getTickRateMilliseconds =
  importProc "ivory_grtos_time_gettickrate_ms" timeWrapperHeader
