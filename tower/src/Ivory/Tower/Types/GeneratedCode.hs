{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , GeneratedSignal(..)
  , generatedCodeForSignal
  ) where

import qualified Data.Map as Map
import Data.Monoid
import qualified Ivory.Tower.AST as AST
import Ivory.Language
import Ivory.Artifact
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.MonitorCode

data GeneratedCode = GeneratedCode
  { generatedcode_modules   :: [Module]
  , generatedcode_depends   :: [Module]
  , generatedcode_threads   :: Map.Map AST.Thread ThreadCode
  , generatedcode_monitors  :: Map.Map AST.Monitor MonitorCode
  , generatedcode_signals   :: Map.Map String GeneratedSignal
  , generatedcode_init      :: forall eff. Ivory eff ()
  , generatedcode_artifacts :: [Artifact]
  }

instance Monoid GeneratedCode where
  mempty = GeneratedCode
    { generatedcode_modules   = []
    , generatedcode_depends   = []
    , generatedcode_threads   = Map.singleton initThread mempty
    , generatedcode_monitors  = Map.empty
    , generatedcode_signals   = Map.empty
    , generatedcode_init      = return ()
    , generatedcode_artifacts = []
    }
    where
    initThread = AST.InitThread AST.Init

  mappend a b = GeneratedCode
    { generatedcode_modules = generatedcode_modules a `mappend` generatedcode_modules b
    , generatedcode_depends = generatedcode_depends a `mappend` generatedcode_depends b
    , generatedcode_threads = Map.unionWith mappend (generatedcode_threads a) (generatedcode_threads b)
    , generatedcode_monitors = Map.unionWith mappend (generatedcode_monitors a) (generatedcode_monitors b)
    , generatedcode_signals = generatedcode_signals a `Map.union` generatedcode_signals b
    , generatedcode_init = generatedcode_init a >> generatedcode_init b
    , generatedcode_artifacts = generatedcode_artifacts a `mappend` generatedcode_artifacts b
    }

newtype GeneratedSignal =
  GeneratedSignal
    { unGeneratedSignal :: (forall eff . Ivory eff ()) -> ModuleDef
    -- ^ Unsafe signal continuation.
    }

generatedCodeForSignal :: AST.Signal -> GeneratedCode
                       -> GeneratedSignal
generatedCodeForSignal sig gc = maybe err id lkup
  where
  lkup = Map.lookup (AST.signal_name sig) (generatedcode_signals gc)
  err = error ("generateCodeForSignal failed: could not find signal code for "
                ++ "signal named " ++ AST.signal_name sig)
