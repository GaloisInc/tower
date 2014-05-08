{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Types.Event
  ( Event(..)
  , eventDescription
  ) where

import Ivory.Language
import qualified Ivory.Tower.AST.Event as AST

data Event (area :: Area *) =
  Event
    { evt_get :: forall s eff
               . Ref s area
              -> Ivory (AllocEffects eff) IBool
    , evt_ast :: AST.Event
    }

eventDescription :: Event a -> String
eventDescription = AST.eventName . evt_ast
