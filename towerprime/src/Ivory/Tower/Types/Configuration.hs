{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Types.Configuration
  ( Configuration(..)
  ) where

import Ivory.Language

class Configuration p where
  type SignalType p
