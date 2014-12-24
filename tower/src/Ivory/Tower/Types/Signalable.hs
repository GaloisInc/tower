{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  ) where

import Ivory.Language

class Signalable s where
  signalName    :: s -> String
  signalHandler :: s -> (forall eff . Ivory eff ()) -> ModuleDef

  -- | Code to prepare this signal for use, if necessary. The default
  -- implementation emits no code.
  signalInit :: s -> Ivory eff ()
  signalInit _ = return ()
