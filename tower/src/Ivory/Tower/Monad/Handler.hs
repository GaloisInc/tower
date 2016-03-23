{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Monad.Handler
  ( Handler(..)
  , Handler'
  , handler
  , handlerName
  , handlerPutASTEmitter
  , handlerPutCodeCallback
  , handlerGetBackend
  , liftMonitor -- XXX UNSAFE TO USE
  -- Source Location
  , mkLocation
  , setLocation
  , withLocation
  ) where

import Prelude ()
import Prelude.Compat

import MonadLib
import Control.Monad.Fix
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.SrcLoc.Location (SrcLoc(..), Position(..), Range(..))

import Ivory.Language
import qualified Ivory.Language.Syntax.AST as IAST
import Ivory.Tower.Backend

data PartialHandler = PartialHandler
  { partialEmitters :: [AST.Emitter]
  , partialComments :: [AST.Comment]
  }

instance Monoid PartialHandler where
  mempty = PartialHandler mempty mempty
  mappend a b = PartialHandler
    { partialEmitters = partialEmitters a `mappend` partialEmitters b
    , partialComments = partialComments a `mappend` partialComments b
    }

newtype Handler area e a = Handler
  { unHandler :: forall backend. TowerBackend backend => Handler' backend area e a
  }
-- GHC can't derive these trivial instances because of the RankNType.

instance Functor (Handler area e) where
  fmap f (Handler h) = Handler $ fmap f h

instance Monad (Handler area e) where
  return x = Handler $ return x
  Handler x >>= f = Handler $ x >>= (unHandler . f)

instance Applicative (Handler area e) where
  pure = return
  (<*>) = ap

instance MonadFix (Handler area e) where
  mfix f = Handler $ mfix (unHandler . f)

newtype Handler' b (area :: Area *) e a = Handler'
  { unHandler' :: ReaderT Unique
                  (WriterT (PartialHandler, [IAST.Proc])
                    (Monitor' b e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

handler :: (IvoryArea a, IvoryZero a)
        => ChanOutput a -> String -> Handler a e () -> Monitor e ()
handler (ChanOutput (Chan chanast)) n b = Monitor $ do
  u <- freshname n
  (r, (part, callbacks)) <- runWriterT $ runReaderT u $ unHandler' $ unHandler b

  let handlerast = AST.Handler u chanast
        (partialEmitters part) (callbacks) (partialComments part) []

  monitorPutHandler handlerast

  return r

handlerName :: Handler a e Unique
handlerName = Handler $ Handler' ask


handlerPutAST :: PartialHandler -> Handler' b a e ()
handlerPutAST part = Handler' $ put (part, mempty)

handlerPutASTEmitter :: AST.Emitter -> Handler' b a e ()
handlerPutASTEmitter a = handlerPutAST $ mempty { partialEmitters = [a] }

handlerPutCodeCallback :: IAST.Proc -> Handler' b a e ()
handlerPutCodeCallback p = Handler' $ put (mempty, [p])

handlerGetBackend :: Handler' backend a e backend
handlerGetBackend = Handler' $ lift $ lift monitorGetBackend

instance BaseUtils (Handler' b a) p where
  freshname n = Handler' $ lift $ lift $ freshname n
  getEnv = Handler' $ lift $ lift getEnv

instance BaseUtils (Handler a) p where
  freshname n = Handler $ freshname n
  getEnv = Handler getEnv

liftMonitor :: Monitor e r -> Handler a e r
liftMonitor a = Handler $ Handler' $ lift $ lift $ unMonitor a

--------------------------------------------------------------------------------
-- SrcLoc stuff

mkLocation :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc
mkLocation file l1 c1 l2 c2
  = SrcLoc (Range (Position 0 l1 c1) (Position 0 l2 c2)) (Just file)

setLocation :: SrcLoc -> Handler a e ()
setLocation l = Handler $ handlerPutAST $ mempty { partialComments = [AST.SourcePos l] }

withLocation :: SrcLoc -> Handler area e a -> Handler area e a
withLocation src h = setLocation src >> h
