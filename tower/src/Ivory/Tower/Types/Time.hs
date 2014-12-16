{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.Types.Time
  ( Time
  , Microseconds(..)
  , Milliseconds(..)
  , toMicroseconds
  , toMilliseconds
  , microseconds
  , us
  , ms

  , ITime
  , fromIMicroseconds
  , fromIMilliseconds
  , toIMicroseconds
  , toIMilliseconds
  , toITime
  , prettyTime
  ) where

import Ivory.Language

class Time a where
  toMicroseconds :: a -> Integer

microseconds :: Time a => a -> Microseconds
microseconds = Microseconds . toMicroseconds

toMilliseconds :: (Time a) => a -> Integer
toMilliseconds t = (toMicroseconds t) `div` 1000

newtype Microseconds = Microseconds Integer deriving (Eq, Show, Ord)
instance Time Microseconds where
  toMicroseconds (Microseconds t) = t

us :: Integer -> Microseconds
us = Microseconds

newtype Milliseconds = Milliseconds Integer deriving (Eq, Show, Ord)
instance Time Milliseconds where
  toMicroseconds (Milliseconds t) = t * 1000

ms :: Integer -> Milliseconds
ms = Milliseconds

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

toITime :: (Time a) => a -> ITime
toITime t = fromIMicroseconds us'
  where
  us' :: Sint64
  us' = fromIntegral (toMicroseconds t)

prettyTime :: Microseconds -> String
prettyTime m = t
  where
  us' = toMicroseconds m
  t  = case us' `mod` 1000 of
    0 -> (show (us' `div` 1000)) ++ "ms"
    _ -> (show us') ++ "us"
