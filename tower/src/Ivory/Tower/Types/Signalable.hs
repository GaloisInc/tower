{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  ) where

import Ivory.Language

class Signalable s where
  signalName    :: s -> String
  signalHandler :: s -> (forall eff . Ivory eff ()) -> ModuleDef

