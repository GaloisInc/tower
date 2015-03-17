{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Codegen
  ( Codegen
  , runCodegen
  , codegenModule
  , codegenDepends
  , codegenThreadCode
  , codegenMonitor
  , codegenSignal
  , codegenArtifact
  ) where

import MonadLib
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.Monoid
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Signalable
import Ivory.Tower.Monad.Base
import qualified Ivory.Tower.AST as AST

import Ivory.Language
import Ivory.Artifact

newtype Codegen env a = Codegen
  { unCodegen :: ReaderT AST.Tower (WriterT GeneratedCode (Base env)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runCodegen :: Codegen env a -> AST.Tower -> Base env (a, GeneratedCode)
runCodegen m ast = fmap (second dedupArtifacts)
                      $ runWriterT
                      $ runReaderT ast (unCodegen m)

codegenModule :: Module -> Codegen e ()
codegenModule m = Codegen $ put $ mempty { generatedcode_modules = [m] }

codegenDepends :: Module -> Codegen e ()
codegenDepends m = Codegen $ put $ mempty { generatedcode_depends = [m] }

codegenThreadCode :: (AST.Tower -> [(AST.Thread, ThreadCode)]) -> Codegen e ()
codegenThreadCode f = Codegen $ do
  a <- ask
  put $ mempty { generatedcode_threads = Map.fromListWith mappend $ f a }

codegenMonitor :: AST.Monitor -> MonitorCode -> Codegen e ()
codegenMonitor m mc = Codegen $ put $
  mempty { generatedcode_monitors = Map.singleton m mc }

codegenSignal :: (Signalable s) => s -> (forall eff . Ivory eff ())
              -> Codegen e ()
codegenSignal s i = Codegen $ put $ mempty
  { generatedcode_init = signalInit s
  , generatedcode_signals = Map.singleton (signalName s) $
      GeneratedSignal $ \i' -> signalHandler s (i >> i')
  }

codegenArtifact :: Artifact -> Codegen e ()
codegenArtifact a = Codegen $ put $ mempty { generatedcode_artifacts = [a] }

instance BaseUtils Codegen e where
  fresh = Codegen $ lift $ lift fresh
  getEnv = Codegen $ lift $ lift getEnv
