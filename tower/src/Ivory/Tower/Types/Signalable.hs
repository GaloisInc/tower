{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  ) where

import Ivory.Language

class Signalable p where
  data SignalType p
  signalName :: SignalType p -> String
  signalHandler :: SignalType p -> (forall eff . Ivory eff ()) -> ModuleDef

