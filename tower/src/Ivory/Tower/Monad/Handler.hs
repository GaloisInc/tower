{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , handler
  , handlerName
  , handlerPutASTEmitter
  , handlerPutASTCallback
  , handlerPutCodeEmitter
  , handlerPutCodeCallback
  , liftMonitor -- XXX UNSAFE TO USE
  -- Source Location
  , mkLocation
  , setLocation
  , withLocation
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative
import Data.Monoid
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Codegen.Handler
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.SrcLoc.Location (SrcLoc(..), Position(..), Range(..))

import Ivory.Language

data PartialHandler = PartialHandler
  { partialEmitters :: [AST.Emitter]
  , partialCallbacks :: [Unique]
  , partialComments :: [AST.Comment]
  }

instance Monoid PartialHandler where
  mempty = PartialHandler mempty mempty mempty
  mappend a b = PartialHandler
    { partialEmitters = partialEmitters a `mappend` partialEmitters b
    , partialCallbacks = partialCallbacks a `mappend` partialCallbacks b
    , partialComments = partialComments a `mappend` partialComments b
    }

newtype Handler (area :: Area *) e a = Handler
  { unHandler :: ReaderT Unique
                  (WriterT (PartialHandler, HandlerCode area)
                    (Monitor e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

handler :: (IvoryArea a, IvoryZero a)
        => ChanOutput a -> String -> Handler a e () -> Monitor e ()
handler (ChanOutput (Chan chanast)) n b = do
  u <- freshname n
  (r, (part, hc)) <- runWriterT $ runReaderT u $ unHandler b

  let handlerast = AST.Handler u chanast
        (partialEmitters part) (partialCallbacks part) (partialComments part)

  monitorPutASTHandler handlerast
  monitorPutThreadCode $ \twr ->
    generateHandlerThreadCode hc twr handlerast

  return r

handlerName :: Handler a e Unique
handlerName = Handler ask

handlerPutAST :: PartialHandler -> Handler a e ()
handlerPutAST part = Handler $ put (part, mempty)

handlerPutCode :: HandlerCode a -> Handler a e ()
handlerPutCode hc = Handler $ put (mempty, hc)

handlerPutASTEmitter :: AST.Emitter -> Handler a e ()
handlerPutASTEmitter a = handlerPutAST $ mempty { partialEmitters = [a] }

handlerPutASTCallback :: Unique -> Handler a e ()
handlerPutASTCallback a = handlerPutAST $ mempty { partialCallbacks = [a] }

handlerPutCodeCallback :: (forall s. AST.Thread -> (Def ('[ConstRef s a] :-> ()), ModuleDef))
                       -> Handler a e ()
handlerPutCodeCallback ms = handlerPutCode $
  mempty { handlercode_callbacks = \ t -> let (p, d) = ms t in ([p], d) }

handlerPutCodeEmitter :: (AST.Tower -> AST.Thread -> EmitterCode b)
                      -> Handler a e ()
handlerPutCodeEmitter ms = handlerPutCode $
  mempty { handlercode_emitters = \ twr thd -> [SomeEmitterCode $ ms twr thd] }

instance BaseUtils (Handler a) p where
  fresh  = liftMonitor fresh
  getEnv = liftMonitor getEnv

liftMonitor :: Monitor e r -> Handler a e r
liftMonitor a = Handler $ lift $ lift a

--------------------------------------------------------------------------------
-- SrcLoc stuff

mkLocation :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc
mkLocation file l1 c1 l2 c2
  = SrcLoc (Range (Position 0 l1 c1) (Position 0 l2 c2)) (Just file)

setLocation :: SrcLoc -> Handler a e ()
setLocation l = handlerPutAST $ mempty { partialComments = [AST.SourcePos l] }

withLocation :: SrcLoc -> Handler area e a -> Handler area e a
withLocation src h = setLocation src >> h
