{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.EventLoop where

import Ivory.Language

import Ivory.Tower.Types

-- Public EventLoop interface

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (eff `AllocsIn` cs, IvoryArea area, IvoryZero area)
    => ChannelReceiver area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel rxer k = EventLoop [ \sch -> sch_mkReceiver sch rxer k ]

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (eff `AllocsIn` cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per k = EventLoop [ \sch -> sch_mkPeriodic sch per k ]

eventLoop :: (eff `AllocsIn` cs ) => Schedule -> EventLoop eff -> Ivory eff ()
eventLoop sch el = sch_mkEventLoop sch [ event sch | event <- unEventLoop el ]
