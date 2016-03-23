module Ivory.Tower.Types.Opts where

import Prelude
import Data.List (find)

-- This file contains the Tower Opts declarations defined in tower-opts

elemOpt :: Opt -> [Opt] -> Bool
elemOpt = any . (===)

getOpt :: Opt -> [Opt] -> Maybe Opt
getOpt needle stack = find ((===) needle) stack 

(===) :: Opt -> Opt -> Bool
(===) (LockCoarsening _) (LockCoarsening _) = True
-- (===) _ _ = False


-- add an entry for each opt added in the tower-opt directory
data Opt = LockCoarsening OptLockCoarsening
  deriving (Show, Read, Eq, Ord)

-- the Lock Coarsening decoration for the AST
data OptLockCoarsening = OptVoid
                       | OptTower
                       | OptMonitor [[String]]
                       | OptHandler [String]
  deriving (Show, Read, Eq, Ord)