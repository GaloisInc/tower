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
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Signalable
import Ivory.Tower.Monad.Base
import qualified Ivory.Tower.AST as AST

import Ivory.Language
import Ivory.Artifact

newtype Codegen env a = Codegen
  { unCodegen :: ReaderT AST.Tower (StateT GeneratedCode (Base env)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runCodegen :: Codegen env a -> AST.Tower -> Base env (a, GeneratedCode)
runCodegen m ast = runStateT emptyGeneratedCode
                      $ runReaderT ast (unCodegen m)

getAST :: Codegen e AST.Tower
getAST = Codegen ask

withGeneratedCode :: (GeneratedCode -> GeneratedCode) -> Codegen e ()
withGeneratedCode f = Codegen $ do
  gc <- get
  set (f gc)

codegenModule :: Module -> Codegen e ()
codegenModule m =
  withGeneratedCode $ \c -> generatedCodeInsertModule m c

codegenDepends :: Module -> Codegen e ()
codegenDepends m =
  withGeneratedCode $ \c -> generatedCodeInsertDepends m c

codegenThreadCode :: (AST.Tower -> [ThreadCode]) -> Codegen e ()
codegenThreadCode f = do
  a <- getAST
  -- Don't replace this fold with a mapM - causes black hole
  withGeneratedCode $ \c ->
    foldl (flip generatedCodeInsertThreadCode) c (f a)

codegenMonitor :: AST.Monitor -> MonitorCode -> Codegen e ()
codegenMonitor m mc =
  withGeneratedCode $ generatedCodeInsertMonitorCode m mc

codegenSignal :: (Signalable s) => s -> (forall eff . Ivory eff ())
              -> Codegen e ()
codegenSignal s i = withGeneratedCode $
  generatedCodeInsertInitCode (signalInit s) .
  generatedCodeInsertSignalCode (signalName s) (\i' -> (signalHandler s) (i >> i'))

codegenArtifact :: Artifact -> Codegen e ()
codegenArtifact = withGeneratedCode . generatedCodeInsertArtifact

instance BaseUtils Codegen e where
  fresh = Codegen $ lift $ lift fresh
  getEnv = Codegen $ lift $ lift getEnv
