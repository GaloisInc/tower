{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Task
  ( Task
  , task
  ) where

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

