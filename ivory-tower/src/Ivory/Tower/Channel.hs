{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Channel where

import Ivory.Language
import Ivory.Tower.Types

-- | Nonblocking emit. Fails silently.
emit :: (IvoryType area, eff `AllocsIn` cs)
     => Schedule -> ChannelEmitter area -> ConstRef s area -> Ivory eff ()
emit schedule emitter ref = sch_mkEmitter schedule emitter ref

