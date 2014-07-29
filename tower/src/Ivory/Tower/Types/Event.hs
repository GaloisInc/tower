{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Types.Event
  ( Event(..)
  , eventDescription
  ) where

import Ivory.Language
import qualified Ivory.Tower.AST as AST

data Event (area :: Area *) =
  Event
    { evt_get :: forall s eff
               . Ref s area
              -> Ivory (AllocEffects eff) IBool
    , evt_trigger :: AST.Trigger
    }

eventDescription :: Event a -> String
eventDescription = AST.triggerName . evt_trigger
