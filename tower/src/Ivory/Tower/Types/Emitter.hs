{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Emitter where

import Ivory.Language

newtype Emitter (a :: Area *) = Emitter
  { emit :: forall s eff. ConstRef s a -> Ivory eff ()
  }
