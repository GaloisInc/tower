{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.EventLoop where

import Ivory.Language

import Ivory.Tower.Types

-- Internal interface

singEventLoop :: EventLoopImpl eff -> EventLoop eff
singEventLoop eli = EventLoop [eli]

-- Public EventLoop interface

handlers :: EventLoop eff -> Ivory eff (EventLoop eff)
handlers = return

onChannel :: (eff `AllocsIn` cs, IvoryType area, IvoryZero area)
    => ScheduledReceiver area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel ur c = singEventLoop $ EventLoopChannel ur c

onTimer :: (eff `AllocsIn` cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per c = singEventLoop $ EventLoopPeriod (unPeriod per) c

