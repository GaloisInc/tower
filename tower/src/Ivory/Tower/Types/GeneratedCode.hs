{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.GeneratedCode
  ( GeneratedCode(..)
  , GeneratedSignal(..)
  , generatedCodeInsertModule
  , generatedCodeInsertDepends
  , generatedCodeInsertThreadCode
  , generatedCodeInsertMonitorCode
  , generatedCodeInsertSignalCode
  , generatedCodeInsertInitCode
  , generatedCodeInsertArtifact
  , generatedCodeForSignal
  , emptyGeneratedCode
  ) where

import qualified Data.Map as Map
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

newtype GeneratedSignal =
  GeneratedSignal
    { unGeneratedSignal :: (forall eff . Ivory eff ()) -> ModuleDef
    -- ^ Unsafe signal continuation.
    }

generatedCodeInsertModule :: Module
                          -> GeneratedCode -> GeneratedCode
generatedCodeInsertModule m g =
  g { generatedcode_modules = m : generatedcode_modules g }

generatedCodeInsertDepends :: Module
                          -> GeneratedCode -> GeneratedCode
generatedCodeInsertDepends m g =
  g { generatedcode_depends = m : generatedcode_depends g }

generatedCodeInsertThreadCode :: ThreadCode
                              -> GeneratedCode -> GeneratedCode
generatedCodeInsertThreadCode tc g =
  g { generatedcode_threads = ins (generatedcode_threads g) }
  where ins = Map.insertWith addThreadCode (threadcode_thread tc) tc

generatedCodeInsertMonitorCode :: AST.Monitor -> MonitorCode
                               -> GeneratedCode -> GeneratedCode
generatedCodeInsertMonitorCode mast mc g =
  g { generatedcode_monitors = ins (generatedcode_monitors g) }
  where ins = Map.insertWith addMonitorCode mast mc

generatedCodeInsertSignalCode :: String
                              -> ((forall eff . Ivory eff ()) -> ModuleDef)
                              -> GeneratedCode -> GeneratedCode
generatedCodeInsertSignalCode signame sigcode g =
  g { generatedcode_signals = ins (generatedcode_signals g) }
  where ins = Map.insert signame (GeneratedSignal sigcode)

generatedCodeInsertInitCode :: (forall eff. Ivory eff ())
                            -> GeneratedCode -> GeneratedCode
generatedCodeInsertInitCode code g =
  g { generatedcode_init = generatedcode_init g >> code }

generatedCodeInsertArtifact :: Artifact
                            -> GeneratedCode -> GeneratedCode
generatedCodeInsertArtifact a g =
  g { generatedcode_artifacts = a : generatedcode_artifacts g }

generatedCodeForSignal :: AST.Signal -> GeneratedCode
                       -> GeneratedSignal
generatedCodeForSignal sig gc = maybe err id lkup
  where
  lkup = Map.lookup (AST.signal_name sig) (generatedcode_signals gc)
  err = error ("generateCodeForSignal failed: could not find signal code for "
                ++ "signal named " ++ AST.signal_name sig)

emptyGeneratedCode :: GeneratedCode
emptyGeneratedCode = GeneratedCode
  { generatedcode_modules   = []
  , generatedcode_depends   = []
  , generatedcode_threads   = Map.fromList
      [ (initThread, emptyThreadCode initThread) ]
  , generatedcode_monitors  = Map.empty
  , generatedcode_signals   = Map.empty
  , generatedcode_init      = return ()
  , generatedcode_artifacts = []
  }
  where
  initThread = AST.InitThread AST.Init
