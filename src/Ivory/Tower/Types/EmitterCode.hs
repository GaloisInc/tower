
module Ivory.Tower.Types.EmitterCode
  ( EmitterCode(..)
  ) where

import Ivory.Tower.ToyObjLang

data EmitterCode = EmitterCode
  { emittercode_init :: Proc
  , emittercode_emit :: Proc
  , emittercode_deliver :: Proc
  , emittercode_user :: ModuleM ()
  , emittercode_gen :: ModuleM ()
  }
