{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Monad.Handler
  ( Handler
  , runHandler
  , handlerName
  , handlerPutASTEmitter
  , handlerPutASTCallback
  , handlerPutCodeEmitter
  , handlerPutCodeCallback
  -- Source Location
  , mkLocation
  , setLocation
  , withLocation
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Codegen.Handler
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.SrcLoc.Location (SrcLoc(..), Position(..), Range(..))

import Ivory.Language

newtype Handler (area :: Area *) e a = Handler
  { unHandler :: StateT AST.Handler
                  (StateT (AST.Tower -> [(AST.Thread, HandlerCode area)])
                    (Monitor e)) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runHandler :: (IvoryArea a, IvoryZero a)
           => String -> AST.Chan -> Handler a e ()
           -> Monitor e ()
runHandler n ch b = mdo
  u <- freshname n
  let h = AST.emptyHandler u ch
  (handlerast, thcs) <- runStateT (emptyHandlerThreadCode handlerast)
                      $ fmap snd (runStateT h (unHandler b))

  monitorPutASTHandler handlerast
  monitorPutThreadCode $ \twr ->
    generateHandlerThreadCode thcs twr handlerast

withAST :: (AST.Handler -> AST.Handler) -> Handler a e ()
withAST f = Handler $ do
  a <- get
  set (f a)

handlerName :: Handler a e Unique
handlerName = Handler $ do
  a <- get
  return (AST.handler_name a)

handlerPutASTEmitter :: AST.Emitter -> Handler a e ()
handlerPutASTEmitter a = withAST (AST.handlerInsertEmitter a)

handlerPutASTCallback :: Unique -> Handler a e ()
handlerPutASTCallback a = withAST (AST.handlerInsertCallback a)

withCode :: (AST.Tower -> AST.Thread -> HandlerCode a -> HandlerCode a)
         -> Handler a e ()
withCode f = Handler $ do
  tcs <- lift get
  lift (set (\twr -> [(t, f twr t c) | (t, c) <- tcs twr ]))

handlerPutCodeCallback :: (AST.Thread -> ModuleDef)
                       -> Handler a e ()
handlerPutCodeCallback ms = withCode $ \_ t -> insertHandlerCodeCallback (ms t)

handlerPutCodeEmitter :: (AST.Tower -> AST.Thread -> EmitterCode b)
                      -> Handler a e ()
handlerPutCodeEmitter ms = withCode $ \a t -> insertHandlerCodeEmitter (ms a t)

instance BaseUtils (Handler a) p where
  fresh  = Handler $ lift $ lift fresh
  getEnv = Handler $ lift $ lift getEnv

--------------------------------------------------------------------------------
-- SrcLoc stuff

mkLocation :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc
mkLocation file l1 c1 l2 c2
  = SrcLoc (Range (Position 0 l1 c1) (Position 0 l2 c2)) (Just file)

setLocation :: SrcLoc -> Handler a e ()
setLocation src = withAST (AST.handlerInsertComment (AST.SourcePos src))

withLocation :: SrcLoc -> Handler area e a -> Handler area e a
withLocation src h = setLocation src >> h
