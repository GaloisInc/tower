{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Generated
  ( Generated
  , runGenerated
  , codegenPutModules
  , codegenPutThreadCode
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Monad.Base
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Generated a = Generated
  { unGenerated :: ReaderT AST.Tower (StateT GeneratedCode Base) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runGenerated :: Generated a -> AST.Tower -> Base (a, GeneratedCode)
runGenerated m ast = runStateT emptyGeneratedCode
                      $ runReaderT ast (unGenerated m)

codegenPutModules :: (AST.Tower -> [Module]) -> Generated ()
codegenPutModules f = Generated $ do
  ms <- asks f
  gc <- get
  set (gc { generatedcode_modules = generatedcode_modules gc ++ ms })

codegenPutThreadCode :: (AST.Tower -> [(AST.Thread, ModuleM ())])
                     -> Generated ()
codegenPutThreadCode f = Generated $ do
  tms <- asks f
  gc <- get
  set (foldl gen gc tms)
  where
  gen gc (thread, moddef) = generatedCodeForThread thread moddef gc

instance BaseUtils Generated where
  fresh = Generated $ lift $ lift fresh
