{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Base
  ( Base
  , BaseUtils(..)
  , runBase
  , freshname
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Ivory.Tower.Types.Unique

newtype Base env a = Base
  { unBase :: StateT Integer (ReaderT env Id) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runBase :: env -> Base env a -> a
runBase env b = runId
              $ runReaderT env
              $ fmap fst
              $ runStateT 0
              $ unBase b

class (Monad m) => BaseUtils m where
  fresh :: m Integer

instance BaseUtils (Base env) where
  fresh = Base $ do
    n <- get
    set (n + 1)
    return n

freshname :: (BaseUtils m) => String -> m Unique
freshname n = do
  f <- fresh
  return (Unique { unique_name = n, unique_fresh = f })


