{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Tower
  ( Tower
  , SystemCode(..)
  , runTower

  , putTaskCode
  , putSysModdef
  , putSysCommInitializer
  , putArtifact
  , putModule
  , putChan
  , putTask

  , group

  -- Internal only!
  , towerLiftBase
  , getSystemCode
  , setSystemCode

  ) where

import MonadLib
import Control.Applicative (Applicative)

import Ivory.Language hiding (local)
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.Directory as D
import Ivory.Tower.Types.Artifact
import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.SystemCode
import Ivory.Tower.Types.TaskCode
import Ivory.Tower.Monad.Base

newtype Tower p a = Tower
  { unTower :: StateT (AST.System p) (ReaderT [Unique] (SystemCodegen p)) a
  } deriving (Functor, Monad, Applicative)

newtype SystemCodegen p a = SystemCodegen
  { unSystemCodegen :: StateT (AST.System p -> SystemCode) Base a
  } deriving (Functor, Monad, Applicative)


runTower :: Tower p () -> Base (AST.System p, SystemCode)
runTower t = do
  ((_,s),c) <- runCodegen $ runSystem $ unTower t
  return (s, c s)
  where
  runSystem m = runReaderT [] $ runStateT emptysys m
  runCodegen g = runStateT (const emptycode) (unSystemCodegen g)
  emptycode :: SystemCode
  emptycode = SystemCode
    { systemcode_tasks = []
    , systemcode_moddef = return ()
    , systemcode_comm_initializers = return ()
    , systemcode_modules  = []
    , systemcode_artifacts = []
    }
  emptysys :: AST.System p
  emptysys = AST.System
    { AST.system_channels = []
    , AST.system_tasks = D.empty
    }

instance BaseUtils (Tower p) where
  getOS = towerLiftBase getOS
  fresh = towerLiftBase fresh


towerLiftBase :: Base a
              -> Tower p a
towerLiftBase t = Tower $ lift $ lift $ SystemCodegen $ lift t

-- Internal API to SystemCodegen

getSystemCode :: Tower p (AST.System p -> SystemCode)
getSystemCode = Tower (lift $ lift $ SystemCodegen get)

setSystemCode :: (AST.System p -> SystemCode) -> Tower p ()
setSystemCode c = Tower (lift $ lift $ SystemCodegen $ set c)

putTaskCode :: (AST.System p -> TaskCode) -> Tower p ()
putTaskCode cgen = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_tasks =
                                      (cgen sys) : systemcode_tasks (c sys) }

putSysModdef :: (AST.System p -> ModuleDef) -> Tower p ()
putSysModdef m = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_moddef =
                                      m sys >> systemcode_moddef (c sys) }

putSysCommInitializer :: (forall s . AST.System p -> Ivory (AllocEffects s) ())
                      -> Tower p ()
putSysCommInitializer i = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_comm_initializers =
                    i sys >> systemcode_comm_initializers (c sys) }

putModule :: Module -> Tower p ()
putModule m = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_modules =
                    m : systemcode_modules (c sys) }

putArtifact :: Artifact -> Tower p ()
putArtifact a = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_artifacts =
                    a : systemcode_artifacts (c sys) }

-- Internal API to AST

getAST :: Tower p (AST.System p)
getAST = Tower get

setAST :: AST.System p -> Tower p ()
setAST a = Tower $ set a

getScope :: Tower p [Unique]
getScope = Tower ask

localScope :: [Unique] -> Tower p a -> Tower p a
localScope s t = Tower $ local s (unTower t)

putChan :: AST.Chan -> Tower p ()
putChan c = do
  a <- getAST
  setAST $ a { AST.system_channels = c : AST.system_channels a }

putTask :: AST.Task p -> Tower p ()
putTask t = do
  a <- getAST
  scope <- getScope
  setAST $ a { AST.system_tasks = D.insert scope t (AST.system_tasks a) }

group :: String -> Tower p a -> Tower p a
group name t = do
  s <- getScope
  g <- freshname name
  localScope (s ++ [g]) t



