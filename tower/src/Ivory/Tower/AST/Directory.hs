{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Ivory.Tower.AST.Directory
  ( Dir(..)
  , Subdir(..)
  , empty
  , insert
  , flatten
  , lookup
  , find
  ) where

import Prelude hiding (lookup)
import qualified Prelude as P (lookup)
import Data.Tuple (swap)

-- import Data.List (sort)
-- import Test.QuickCheck

data Dir s a = Dir [a] [Subdir s a] deriving (Eq, Show)
data Subdir s a = Subdir s (Dir s a) deriving (Eq, Show)

flatten :: Dir s a -> [([s],a)]
flatten a = auxD [] a
  where
  auxD p (Dir as ss) = (map (\v -> (reverse p,v)) as) ++ (concatMap (auxS p) ss)
  auxS p (Subdir s d)  = auxD (s:p) d

empty :: Dir s a
empty = Dir [] []

insert :: (Eq s) => [s] -> a -> Dir s a -> Dir s a
insert []     a (Dir as ss) = Dir (a:as) ss
insert (p:ps) a (Dir as ss) = Dir as $ case finddirectory p ss of
  (Just d, ss') -> (Subdir p (insert ps a d)):ss'
  (Nothing, _)  ->  (Subdir p (create ps a)):ss

finddirectory :: (Eq s) => s -> [Subdir s a] -> (Maybe (Dir s a), [Subdir s a])
finddirectory dir = foldl aux (Nothing,[])
  where
  aux (f,ss) s@(Subdir d a) | d == dir = (Just a, ss)
                            | otherwise = (f,s:ss)

create :: [s] -> a -> Dir s a
create (p:ps) a = Dir []  [Subdir p (create ps a)]
create []     a = Dir [a] []

lookup :: (Eq s) => [s] -> Dir s a -> [a]
lookup (p:ps) (Dir _ ss) = case finddirectory p ss of
  (Just d, _) -> lookup ps d
  (Nothing,_) -> []
lookup []     (Dir as _) = as

find :: (Eq a) => a -> Dir s a -> Maybe [s]
find a d = P.lookup a (map swap (flatten d))

-- suprisingly enough, this code took me a little while to get correct.

-- test = quickCheck (\lap -> sort (flatten (insertpairs lap)) == sort lap)
--  where
--  insertpairs :: [([Char],Int)] -> Dir Char Int
--  insertpairs = foldl aux empty
--    where
--    aux n (k,v) = insert k v n

