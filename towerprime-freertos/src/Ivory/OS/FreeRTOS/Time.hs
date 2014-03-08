{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Time
  ( delay
  , delayUntil
  , getTimeMillis
  , getTimeTick
  , millisToTicks
  ) where

import Ivory.Language

timeWrapperHeader :: String
timeWrapperHeader = "freertos_time_wrapper.h"

delay :: Def ('[ Uint32 ] :->())
delay = importProc "ivory_freertos_time_delay" timeWrapperHeader

delayUntil :: Def ('[ Ref s (Stored Uint32), Uint32 ] :->())
delayUntil = importProc "ivory_freertos_time_delayuntil" timeWrapperHeader

getTimeMillis :: Def ('[] :-> Uint32)
getTimeMillis =
  importProc "ivory_freertos_time_getmilliscount" timeWrapperHeader

getTimeTick :: Def ('[] :-> Uint32)
getTimeTick = importProc "ivory_freertos_time_gettickcount" timeWrapperHeader

millisToTicks :: Def ('[Uint32] :-> Uint32)
millisToTicks = importProc "ivory_freertos_time_millistoticks" timeWrapperHeader
