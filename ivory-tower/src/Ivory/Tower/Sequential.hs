{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Sequential
  ( Runnable
  , sequential
  , begin
  , active
  -- AST Exports
  , Stmt
  , liftIvory
  , check
  , send
  -- Monad exports
  , Sequential()
  , local
  , localInit
  , start
  , receive
  , delay
  , end
  ) where

import Ivory.Language hiding (local, localInit)

import Ivory.Tower.Types
import Ivory.Tower.Node
import Ivory.Tower.Task
import Ivory.Tower.Ivory hiding (receive)

import Ivory.Tower.Sequential.AST hiding (start, end)
import Ivory.Tower.Sequential.Compile
import Ivory.Tower.Sequential.Monad

begin :: Runnable -> Ivory eff ()
begin = call_ . runnable_begin

active :: Runnable -> Ivory eff IBool
active = call . runnable_active

sequential :: (IvoryArea f, IvoryZero f, IvoryArea t, IvoryZero t, SingI n, SingI m)
    => ChannelSource n t
    -> ChannelSink m f
    -> String
    -> Sequential p f t ()
    -> Task p Runnable
sequential source sink name m = do
  e  <- withChannelEmitter  source ("sequentialTxer" ++ name)
  r  <- withChannelReceiver sink   ("sequentialRxer" ++ name)
  sm <- runSequentialMonad m
  n  <- freshname
  millis <- withGetTimeMillis
  -- Need to turn SM into Runnable, recieve callback, and ModuleDef (e.g. generated code)
  compileSM sm e r n millis

