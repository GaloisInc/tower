{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  , SomeHandlerCode(..)
  , somehandlercode_handlername
  , somehandlercode_moddef
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

-- need this for when we make a list of handlercodes and don't care about erasing the area:
data SomeHandlerCode = forall area . SomeHandlerCode { unSomeHandlerCode :: HandlerCode area }

somehandlercode_handlername :: SomeHandlerCode -> Unique
somehandlercode_handlername (SomeHandlerCode h) = handlercode_handlername h

somehandlercode_moddef :: SomeHandlerCode -> ModuleDef
somehandlercode_moddef (SomeHandlerCode h) = handlercode_moddef h

