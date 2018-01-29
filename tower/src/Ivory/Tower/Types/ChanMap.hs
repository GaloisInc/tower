{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Types.ChanMap
  ( ChanMap()
  , empty
  , singleton
  , lookup
  , unionWith
  , keys
  ) where

import qualified Data.Map as Map
import GHC.Exts (Any)
import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Chan
import Prelude hiding (lookup)
import Unsafe.Coerce

newtype ChanMap (v :: Area * -> *) = ChanMap (Map.Map AST.Chan Any)

empty :: ChanMap v
empty = ChanMap Map.empty

singleton :: Chan a -> v a -> ChanMap v
singleton (Chan chan) v = ChanMap $ Map.singleton chan $ unsafeCoerce v

lookup :: Chan a -> ChanMap v -> Maybe (v a)
lookup (Chan chan) (ChanMap m) = fmap unsafeCoerce $ Map.lookup chan m

unionWith :: (v a -> v a -> v a) -> ChanMap v -> ChanMap v -> ChanMap v
unionWith f (ChanMap a) (ChanMap b) = ChanMap $ Map.unionWith (liftDyn f) a b

keys :: ChanMap v -> [AST.Chan]
keys (ChanMap m) = Map.keys m

liftDyn :: (a -> a -> a) -> Any -> Any -> Any
liftDyn f a b = unsafeCoerce $ f (unsafeCoerce a) (unsafeCoerce b)
