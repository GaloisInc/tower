{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monitor
  ( handler
  , state
  , stateInit
  , monitorModuleDef
  , Handler()
  , Monitor()
  ) where

import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Base

import Ivory.Language

handler :: (IvoryArea a, IvoryZero a)
        => ChanOutput a -> String -> Handler a e () -> Monitor e ()
handler (ChanOutput (Chan chanast)) name block = do
  runHandler name chanast block

state :: (IvoryArea a, IvoryZero a)
      => String -> Monitor e (Ref Global a)
state n = state' n Nothing

stateInit :: (IvoryArea a, IvoryZero a)
          => String -> Init a -> Monitor e (Ref Global a)
stateInit n i = state' n (Just i)

state' :: (IvoryArea a, IvoryZero a)
       => String
       -> Maybe (Init a)
       -> Monitor e (Ref Global a)
state' n i = do
  f <- freshname n
  let a = area (showUnique f) i
  monitorPutCode $ \_ -> defMemArea a
  return (addrOf a)


monitorModuleDef :: ModuleDef -> Monitor e ()
monitorModuleDef = monitorPutCode . const
