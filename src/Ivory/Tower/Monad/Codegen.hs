{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Codegen
  ( Codegen
  , runCodegen
  , codegenModules
  , codegenThreadCode
  , codegenMonitor
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Monad.Base
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Codegen a = Codegen
  { unCodegen :: ReaderT AST.Tower (StateT GeneratedCode Base) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runCodegen :: Codegen a -> AST.Tower -> Base (a, GeneratedCode)
runCodegen m ast = runStateT emptyGeneratedCode
                      $ runReaderT ast (unCodegen m)

getAST :: Codegen AST.Tower
getAST = Codegen ask

withGeneratedCode :: (GeneratedCode -> GeneratedCode) -> Codegen ()
withGeneratedCode f = Codegen $ do
  gc <- get
  set (f gc)

-- XXX nobody uses this function, esp not given the AST.Tower continuation.
codegenModules :: (AST.Tower -> [Module]) -> Codegen ()
codegenModules f = do
  a <- getAST
  -- Don't replace this fold with a mapM - causes black hole
  withGeneratedCode $ \c ->
    foldl (flip generatedCodeInsertModule) c (f a)

codegenThreadCode :: (AST.Tower -> [ThreadCode]) -> Codegen ()
codegenThreadCode f = do
  a <- getAST
  -- Don't replace this fold with a mapM - causes black hole
  withGeneratedCode $ \c ->
    foldl (flip generatedCodeInsertThreadCode) c (f a)

-- might not even need AST.Tower continuation exposed here?
codegenMonitor :: AST.Monitor -> (AST.Tower -> MonitorCode) -> Codegen ()
codegenMonitor m f = do
  a <- getAST
  withGeneratedCode $ generatedCodeInsertMonitorCode m (f a)

instance BaseUtils Codegen where
  fresh = Codegen $ lift $ lift fresh
