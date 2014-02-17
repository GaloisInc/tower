{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Tower
  ( Tower
  , SystemCode(..)
  , runTower

  , putTaskCode
  , putSysModdef
  ) where

import MonadLib
import Control.Applicative (Applicative)

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.ATree as ATree
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Task

newtype Tower a = Tower
  { unTower :: StateT (AST.System) SystemCodegen a
  } deriving (Functor, Monad, Applicative)

newtype SystemCodegen a = SystemCodegen
  { unSystemCodegen :: StateT (AST.System -> SystemCode) Base a
  } deriving (Functor, Monad, Applicative)

data SystemCode =
  SystemCode
    { systemcode_tasks :: [TaskCode]
    , systemcode_moddef :: ModuleDef
    }

runTower :: Tower () -> Base (AST.System, SystemCode)
runTower = undefined


-- Internal API to SystemCodegen

getSystemCode :: Tower (AST.System -> SystemCode)
getSystemCode = Tower (lift $ SystemCodegen get)

setSystemCode :: (AST.System -> SystemCode) -> Tower ()
setSystemCode c = Tower (lift $ SystemCodegen $ set c)

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

-- Internal API to AST

getAST :: Tower AST.System
getAST = Tower get

setAST :: AST.System -> Tower ()
setAST a = Tower $ set a

putChan :: AST.Chan -> Tower ()
putChan c = do
  a <- getAST
  setAST $ a { AST.system_channels = c : AST.system_channels a }

putTask :: Unique -> AST.Task -> Tower ()
putTask name t = do
  a <- getAST
  setAST $ a { AST.system_tasks = ATree.insert name t (AST.system_tasks a) }

-- XXX IMPLEMENT SCOPE as environment monad in Tower
