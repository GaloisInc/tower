{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , GeneratedSignal(..)
  , generatedCodeForSignal
  ) where

import qualified Data.Map as Map
import qualified Ivory.Tower.AST as AST
import Ivory.Language
import Ivory.Artifact
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.SignalCode

data GeneratedCode = GeneratedCode
  { generatedcode_modules   :: [Module]
  , generatedcode_depends   :: [Module]
  , generatedcode_threads   :: Map.Map AST.Thread ThreadCode
  , generatedcode_monitors  :: Map.Map AST.Monitor ModuleDef
  , generatedcode_signals   :: Map.Map String GeneratedSignal
  , generatedcode_init      :: forall eff. Ivory eff ()
  , generatedcode_artifacts :: [Located Artifact]
  }

generatedCodeForSignal :: AST.Signal -> GeneratedCode
                       -> GeneratedSignal
generatedCodeForSignal sig gc = maybe err id lkup
  where
  lkup = Map.lookup (AST.signal_name sig) (generatedcode_signals gc)
  err = error ("generateCodeForSignal failed: could not find signal code for "
                ++ "signal named " ++ AST.signal_name sig)
