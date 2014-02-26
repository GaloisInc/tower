{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task
  ( Task
  , task
  , taskLocal
  , taskLocalInit
  ) where

import           Ivory.Language
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task

task :: String
     -> Task ()
     -> Tower ()
task name m = do
  u <- freshname name
  (taskast, codegen) <- runTowerTask m
  putTask u taskast
  putTaskCode codegen

taskLocal :: (IvoryArea area, IvoryZero area)
          => String
          -> Task (Ref Global area)
taskLocal name = do
  u <- freshname name
  let memarea = area (showUnique u) (Just izero)
  putUsercode $ \_ _ -> defMemArea memarea
  return (addrOf memarea)

taskLocalInit :: (IvoryArea area)
              => String
              -> Init area
              -> Task (Ref Global area)
taskLocalInit name iv = do
  u <- freshname name
  let memarea = area (showUnique u) (Just iv)
  putUsercode $ \_ _ -> defMemArea memarea
  return (addrOf memarea)

