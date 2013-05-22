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

-- | Handy syntax to make construction of a 'Ivory.Tower.Tower.taskBody'
--   argument from an Ivory program which returns an 'EventLoop'
handlers :: EventLoop eff -> Ivory eff (EventLoop eff)
handlers = return

-- | Construct an 'EventLoop' from a 'ChannelReceiver' and a handler function
--   which takes a ConstRef to the received event and performs an Ivory
--   computation
onChannel :: (eff `AllocsIn` cs, IvoryType area, IvoryZero area)
    => ChannelReceiver area -> (ConstRef (Stack cs) area -> Ivory eff ())
    -> EventLoop eff
onChannel ur c = singEventLoop $ EventLoopChannel ur c

-- | Construct an 'EventLoop' from a 'Period' and a handler function which
--   takes the current time (in milliseconds) and performs an Ivory computation
onTimer :: (eff `AllocsIn` cs)
    => Period -> (Uint32 -> Ivory eff ())
    -> EventLoop eff
onTimer per c = singEventLoop $ EventLoopPeriod (unPeriod per) c

