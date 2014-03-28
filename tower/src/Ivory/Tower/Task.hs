{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task
  ( Task
  , task
  , taskInit
  , taskLocal
  , taskLocalInit
  , taskModuleDef
  ) where

import           Ivory.Language
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task

task :: String
     -> Task p ()
     -> Tower p ()
task name m = do
  u <- freshname name
  (taskast, codegen) <- towerLiftBase (runTask m u)
  putTask taskast
  putTaskCode codegen

taskInit :: (forall s . Ivory (AllocEffects s) ()) -> Task p ()
taskInit c = putUserInitCode (const c)

taskLocal :: (IvoryArea area, IvoryZero area)
          => String
          -> Task p (Ref Global area)
taskLocal name = do
  u <- freshname name
  let memarea = area (showUnique u) (Just izero)
  putUsercode $ defMemArea memarea
  return (addrOf memarea)

taskLocalInit :: (IvoryArea area)
              => String
              -> Init area
              -> Task p (Ref Global area)
taskLocalInit name iv = do
  u <- freshname name
  let memarea = area (showUnique u) (Just iv)
  putUsercode $ defMemArea memarea
  return (addrOf memarea)

taskModuleDef :: ModuleDef -> Task p ()
taskModuleDef = putUsercode
