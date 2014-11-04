{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Time
  ( delay
  , delayUntil
  , getTickCount
  , getTickRateMilliseconds
  , moddef
  ) where

import Ivory.Language

moddef :: ModuleDef
moddef = inclHeader timeWrapperHeader

timeWrapperHeader :: String
timeWrapperHeader = "freertos_time_wrapper.h"

type Ticks = Uint32

delay :: Def ('[ Ticks ] :->())
delay = importProc "ivory_freertos_time_delay" timeWrapperHeader

delayUntil :: Def ('[ Ref s (Stored Ticks), Ticks ] :->())
delayUntil = importProc "ivory_freertos_time_delayuntil" timeWrapperHeader

getTickCount:: Def ('[] :-> Ticks)
getTickCount =
  importProc "ivory_freertos_time_gettickcount" timeWrapperHeader

getTickRateMilliseconds :: Def ('[] :-> Uint32)
getTickRateMilliseconds =
  importProc "ivory_freertos_time_gettickrate_ms" timeWrapperHeader
