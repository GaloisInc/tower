{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.SignalCode
  ( SignalCode(..)
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Signalable

data SignalCode p =
  SignalCode
    { signalcode_moddef   :: AST.System p -> ModuleDef
    , signalcode_init     :: forall eff . AST.System p -> Ivory eff ()
    , signalcode_receiver :: forall eff
                           . AST.System p
                          -> AST.SignalReceiver (SignalType p)
                          -> Ivory eff IBool
    }
