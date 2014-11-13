{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

class (Monad (m e), Functor (m e)) => BaseUtils m e where
  fresh :: m e Integer
  getEnv :: m e e

instance BaseUtils Base env where
  fresh = Base $ do
    n <- get
    set (n + 1)
    return n
  getEnv = Base $ lift $ ask

freshname :: (BaseUtils m e) => String -> m e Unique
freshname n = do
  f <- fresh
  return (Unique { unique_name = n, unique_fresh = f })


