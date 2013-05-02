{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Channel where

import Ivory.Language
import Ivory.Tower.Types

untypedChannel :: ChannelRef area -> UTChannelRef
untypedChannel = unChannelRef

-- Nonblocking emit. Fails silently.
emit :: (IvoryType area, eff `AllocsIn` cs)
     => ChannelEmitter area -> ConstRef s area -> Ivory eff ()
emit chEmitter r = unChannelEmitter chEmitter r

channelNameForEndpoint :: UTChannelRef -> TaskSchedule -> CompiledChannelName
channelNameForEndpoint utchref sch = ChannelName utchref (tasksch_name sch)

