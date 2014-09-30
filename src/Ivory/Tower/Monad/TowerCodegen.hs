{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.TowerCodegen
  ( TowerCodegen
  , runTowerCodegen
  , codegenPutModule
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

codegenPutModule :: (AST.Tower -> Module) -> TowerCodegen ()
codegenPutModule f = TowerCodegen $ do
  m <- asks f
  s <- get
  set (s { towercode_modules = m : towercode_modules s })

instance BaseUtils TowerCodegen where
  fresh = TowerCodegen $ lift $ lift fresh
