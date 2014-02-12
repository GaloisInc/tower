{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Types.Time
  ( Time
  , fromMicroseconds
  , fromMilliseconds
  , toMicroseconds
  ) where

import Ivory.Language

newtype Time = Time Sint64
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, IvoryIntegral, IvoryStore, IvoryInit)

fromMicroseconds :: (SafeCast a Sint64) => a -> Time
fromMicroseconds = Time . safeCast

fromMilliseconds :: (SafeCast a Sint64) => a -> Time
fromMilliseconds = Time . (*1000) . safeCast

toMicroseconds :: Time -> Sint64
toMicroseconds (Time t) = t

