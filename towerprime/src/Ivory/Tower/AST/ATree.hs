{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Ivory.Tower.AST.ATree
  ( ATree(..)
  ) where

import Data.Foldable
import Data.Traversable

data ATree s a
  = Node s [ATree s a]
  | Leaf a
  deriving (Functor, Foldable, Traversable)
