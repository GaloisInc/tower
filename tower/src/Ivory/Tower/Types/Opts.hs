module Ivory.Tower.Types.Opts where

import Prelude
import Data.List (find)

-- This file contains the Tower Opts declarations defined in tower-opts

elemOpt :: Opt -> [Opt] -> Bool
elemOpt = any . (===)

getOpt :: Opt -> [Opt] -> Maybe Opt
getOpt needle stack = find ((===) needle) stack 

replaceOpt :: Opt -> [Opt] -> [Opt]
replaceOpt needle [] = []
replaceOpt needle (a:b) = if (a === needle) then needle:b else a:(replaceOpt needle b)

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

data FastOpt = FastOpt {opt :: Opt}
  deriving (Show, Read, Ord)

instance Eq FastOpt where
  (==) arg1 arg2 = 
    let a = opt arg1 in
    let b = opt arg2 in
    (===) a b

--TODO write an instance for FastOpt Ord