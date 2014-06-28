{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Base
  ( Base
  , BaseUtils(..)
  , runBase
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative (Applicative)

import Ivory.Tower.Types.OS
import Ivory.Tower.Types.Unique

newtype Base a = Base
  { unBase :: ReaderT OS (StateT Integer Id) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runBase :: Base a -> OS -> a
runBase b os = fst (runM (unBase b) os 0)

class (Monad m) => BaseUtils m where
  getOS :: m OS
  fresh :: m Integer
  freshname :: String -> m Unique
  freshname n = do
    i <- fresh
    return $ Unique { unique_name = n, unique_fresh = i }

instance BaseUtils Base where
  getOS = Base ask
  fresh = Base $ do
    n <- get
    set (n + 1)
    return n

