{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Types.Time
  ( Time
  , Microseconds(..)
  , Milliseconds(..)
  , toMicroseconds
  , toMilliseconds

  , ITime
  , fromIMicroseconds
  , fromIMilliseconds
  , toIMicroseconds
  , toIMilliseconds
  ) where

import Ivory.Language

class Time a where
  toMicroseconds :: a -> Integer

toMilliseconds :: (Time a) => a -> Integer
toMilliseconds t = (toMicroseconds t) `div` 1000

newtype Microseconds = Microseconds Integer
instance Time Microseconds where
  toMicroseconds (Microseconds t) = t

newtype Milliseconds = Milliseconds Integer
instance Time Milliseconds where
  toMicroseconds (Milliseconds t) = t * 1000

newtype ITime = ITime Sint64
  deriving ( Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryIntegral, IvoryStore, IvoryInit, IvoryZeroVal, Bounded)

instance IvorySizeOf (Stored ITime) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Sint64))

fromIMicroseconds :: (SafeCast a Sint64) => a -> ITime
fromIMicroseconds = ITime . safeCast

fromIMilliseconds :: (SafeCast a Sint64) => a -> ITime
fromIMilliseconds = ITime . (*1000) . safeCast

toIMicroseconds :: ITime -> Sint64
toIMicroseconds (ITime t) = t

toIMilliseconds :: ITime -> Sint64
toIMilliseconds (ITime t) = t `iDiv` 1000

