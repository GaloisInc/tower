{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DefaultSignatures #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  ) where

import Ivory.Language

class Signalable s where
  signalName    :: s -> String
  signalHandler :: s -> (forall s' . Ivory (AllocEffects s') ()) -> ModuleDef

  -- | Code to prepare this signal for use, if necessary. The default
  -- implementation emits no code.
  signalInit :: s -> Ivory eff ()
  signalInit _ = return ()

  -- | On some platforms we need access to an underlying IRQ number for
  -- signals that correspond to IRQs.
  signalNumber  :: s -> Int
  default signalNumber :: Enum s => s -> Int
  signalNumber = fromEnum
