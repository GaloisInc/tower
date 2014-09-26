{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Base
  ( Base
  , BaseUtils(..)
  , runBase
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

newtype Base a = Base
  { unBase :: StateT Integer Id a
  } deriving (Functor, Monad, Applicative, MonadFix)

runBase :: Base a -> a
runBase b = fst (runM (unBase b) 0)

class (Monad m) => BaseUtils m where
  fresh :: m Integer

instance BaseUtils Base where
  fresh = Base $ do
    n <- get
    set (n + 1)
    return n

