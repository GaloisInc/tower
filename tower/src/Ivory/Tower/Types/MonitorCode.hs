
module Ivory.Tower.Types.MonitorCode
  ( MonitorCode(..)
  ) where

import Data.Monoid
import Ivory.Language

newtype MonitorCode = MonitorCode
  { monitorcode_moddef :: ModuleDef
  }

instance Monoid MonitorCode where
  mempty = MonitorCode $ return ()
  MonitorCode a `mappend` MonitorCode b = MonitorCode $ a >> b
