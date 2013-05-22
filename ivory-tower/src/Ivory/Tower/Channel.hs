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

-- | Nonblocking emit. Fails silently.
emit :: (IvoryType area, eff `AllocsIn` cs)
     => ChannelEmitter area -> ConstRef s area -> Ivory eff ()
emit _chEmitter _ref = undefined -- XXX fix once we have codegen story

-- | Internal use
-- XXX get rid of this noise
channelNameForEndpoint :: UTChannelRef -> TaskSt -> CompiledChannelName
channelNameForEndpoint utchref sch = ChannelName utchref (taskst_name sch)

