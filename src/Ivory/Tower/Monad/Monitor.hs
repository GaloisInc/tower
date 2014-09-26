{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

newtype Monitor a = Monitor
  { unMonitor :: StateT AST.Monitor Tower a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor () -> Tower AST.Monitor
runMonitor n b = do
  f <- fresh
  let m = AST.emptyMonitor (Unique n f)
  fmap snd (runStateT m (unMonitor b))

instance BaseUtils Monitor where
  fresh = Monitor $ lift fresh
