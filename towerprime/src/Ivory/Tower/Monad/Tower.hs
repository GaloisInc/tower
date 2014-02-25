{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Tower
  ( Tower
  , SystemCode(..)
  , runTower

  , putTaskCode
  , putSysModdef
  , putSysCommInitializer
  , putChan
  , putTask
  , runTowerTask

  , group
  ) where

import MonadLib
import Control.Applicative (Applicative)

import Ivory.Language hiding (local)
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.Directory as D
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Task

newtype Tower a = Tower
  { unTower :: StateT (AST.System) (ReaderT [Unique] SystemCodegen) a
  } deriving (Functor, Monad, Applicative)

newtype SystemCodegen a = SystemCodegen
  { unSystemCodegen :: StateT (AST.System -> SystemCode) Base a
  } deriving (Functor, Monad, Applicative)

data SystemCode =
  SystemCode
    { systemcode_tasks :: [TaskCode]
    , systemcode_moddef :: ModuleDef
    , systemcode_comm_initializers :: forall s . Ivory (AllocEffects s) ()
    }

runTower :: Tower () -> Base (AST.System, SystemCode)
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
    }
  emptysys :: AST.System
  emptysys = AST.System
    { AST.system_channels = []
    , AST.system_tasks = D.empty
    }

instance BaseUtils Tower where
  getOS = Tower $ lift $ lift $ SystemCodegen $ lift getOS
  fresh = Tower $ lift $ lift $ SystemCodegen $ lift fresh

-- Lift of Task.runTask

runTowerTask :: Task () -> Tower (AST.Task, (AST.System -> TaskCode))
runTowerTask t = Tower $ lift $ lift $ SystemCodegen $ lift $ runTask t

-- Internal API to SystemCodegen

getSystemCode :: Tower (AST.System -> SystemCode)
getSystemCode = Tower (lift $ lift $ SystemCodegen get)

setSystemCode :: (AST.System -> SystemCode) -> Tower ()
setSystemCode c = Tower (lift $ lift $ SystemCodegen $ set c)

putTaskCode :: (AST.System -> TaskCode) -> Tower ()
putTaskCode t = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_tasks =
                                      t sys : systemcode_tasks (c sys) }

putSysModdef :: (AST.System -> ModuleDef) -> Tower ()
putSysModdef m = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_moddef =
                                      m sys >> systemcode_moddef (c sys) }

putSysCommInitializer :: (forall s . AST.System -> Ivory (AllocEffects s) ())
                      -> Tower ()
putSysCommInitializer i = do
  c <- getSystemCode
  setSystemCode $ \sys -> (c sys) { systemcode_comm_initializers =
                    i sys >> systemcode_comm_initializers (c sys) }
-- Internal API to AST

getAST :: Tower AST.System
getAST = Tower get

setAST :: AST.System -> Tower ()
setAST a = Tower $ set a

getScope :: Tower [Unique]
getScope = Tower ask

localScope :: [Unique] -> Tower a -> Tower a
localScope s t = Tower $ local s (unTower t)

putChan :: AST.Chan -> Tower ()
putChan c = do
  a <- getAST
  setAST $ a { AST.system_channels = c : AST.system_channels a }

putTask :: Unique -> AST.Task -> Tower ()
putTask name t = do
  a <- getAST
  scope <- getScope
  setAST $ a { AST.system_tasks =
                D.insert (scope ++ [name]) t (AST.system_tasks a) }

group :: String -> Tower a -> Tower a
group name t = do
  s <- getScope
  g <- freshname name
  localScope (s ++ [g]) t



