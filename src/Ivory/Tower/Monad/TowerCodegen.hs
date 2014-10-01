{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.TowerCodegen
  ( TowerCodegen
  , runTowerCodegen
  , codegenPutModules
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.TowerCode
import Ivory.Tower.Monad.Base
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype TowerCodegen a = TowerCodegen
  { unTowerCodegen :: ReaderT AST.Tower (StateT TowerCode Base) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runTowerCodegen :: TowerCodegen a -> AST.Tower -> Base (a, TowerCode)
runTowerCodegen m ast = runStateT emptyTowerCode
                      $ runReaderT ast (unTowerCodegen m)

codegenPutModules :: (AST.Tower -> [Module]) -> TowerCodegen ()
codegenPutModules f = TowerCodegen $ do
  ms <- asks f
  s <- get
  set (s { towercode_modules = towercode_modules s ++ ms })

instance BaseUtils TowerCodegen where
  fresh = TowerCodegen $ lift $ lift fresh
