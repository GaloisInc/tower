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
import Ivory.Tower.Types.Signalable
import Ivory.Tower.Monad.Base

import Ivory.Language
import Ivory.Artifact

newtype Codegen env a = Codegen
  { unCodegen :: WriterT GeneratedCode (Base env) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runCodegen :: Codegen env a -> Base env (a, GeneratedCode)
runCodegen m = fmap (second dedupArtifacts)
                      $ runWriterT
                      $ unCodegen m

codegenModule :: Module -> Codegen e ()
codegenModule m = Codegen $ put $ mempty { generatedcode_modules = [m] }

codegenDepends :: Module -> Codegen e ()
codegenDepends m = Codegen $ put $ mempty { generatedcode_depends = [m] }

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
  fresh = Codegen $ lift fresh
  getEnv = Codegen $ lift getEnv
