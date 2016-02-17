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
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Base

import Ivory.Language

state :: (IvoryArea a, IvoryZero a)
      => String -> Monitor e (Ref 'Global a)
state n = state' n Nothing

stateInit :: (IvoryArea a, IvoryZero a)
          => String -> Init a -> Monitor e (Ref 'Global a)
stateInit n i = state' n (Just i)

state' :: (IvoryArea a, IvoryZero a)
       => String
       -> Maybe (Init a)
       -> Monitor e (Ref 'Global a)
state' n i = do
  f <- freshname n
  let a = area (showUnique f) i
  monitorModuleDef $ defMemArea a
  return (addrOf a)
