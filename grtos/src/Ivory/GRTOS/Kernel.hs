{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.Kernel where

import Ivory.Language

-- Kernel calls. XXX Move these to propre extern defs in another module, use
-- inclheader and sourcedep for c versions.

kernel_wait :: Def('[]:->())
kernel_wait = proc "kernel_wait" $ body $ return ()

kernel_yield :: Def('[]:->())
kernel_yield = proc "kernel_yield" $ body $ return ()

kernel_begin_atomic :: Def('[]:->())
kernel_begin_atomic = proc "kernel_begin_atomic" $ body $ return ()

kernel_end_atomic :: Def('[]:->())
kernel_end_atomic = proc "kernel_end_atomic" $ body $ return ()

