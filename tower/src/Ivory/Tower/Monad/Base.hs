{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Base
  ( Base
  , BaseUtils(..)
  , runBase
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Fix
import qualified Data.Map as Map
import Ivory.Tower.Types.Unique
import MonadLib

newtype Base env a = Base
  { unBase :: StateT (Map.Map String Integer) (ReaderT env Id) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runBase :: env -> Base env a -> a
runBase env b = runId
              $ runReaderT env
              $ fmap fst
              $ runStateT Map.empty
              $ unBase b

class (Monad (m e), Functor (m e)) => BaseUtils m e where
  freshname :: String -> m e Unique
  getEnv :: m e e

instance BaseUtils Base env where
  freshname n = Base $ sets $
    first (Unique n . maybe 1 (+ 1)) . Map.insertLookupWithKey (const (+)) n 1
  getEnv = Base $ lift $ ask
