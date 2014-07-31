{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Unique

data HandlerCode area =
  HandlerCode
    { handlercode_handlername :: Unique
    , handlercode_moddef      :: ModuleDef
    , handlercode_setup       :: forall s . Ivory (AllocEffects s) ()
    , handlercode_callback    :: forall s1 s2 . ConstRef s1 area -> Ivory (AllocEffects s2) ()
    , handlercode_finalizer   :: forall s . Ivory (AllocEffects s) ()
    }
