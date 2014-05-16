{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Task
  ( Task
  , task
  , taskInit
  , taskLocal
  , taskLocalInit
  , taskModuleDef
  , taskChannel
  , taskChannel'
  , taskPriority
  , taskStackSize
  ) where

import GHC.TypeLits

import           Ivory.Language
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Task

import           Ivory.Tower.Types.Channels
import           Ivory.Tower.Channel

task :: String
     -> Task p ()
     -> Tower p ()
task name m = do
  u <- freshname name
  (taskast, codegen) <- runTask m u
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

taskChannel :: (IvoryArea area, IvoryZero area)
            => Task p (ChannelSource area, ChannelSink area)
taskChannel = taskLiftTower channel

taskChannel' :: forall (n :: Nat) p area
              . (ANat n, IvoryArea area, IvoryZero area)
             => Proxy n
             -> Maybe (Init area)
             -> Task p (ChannelSource area, ChannelSink area)
taskChannel' p i = taskLiftTower $ channel' p i

taskPriority :: Integer -> Task p ()
taskPriority = putPriority

taskStackSize :: Integer -> Task p ()
taskStackSize = putStackSize
