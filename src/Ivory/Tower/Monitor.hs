{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monitor
  ( handler
  , state
  , Handler()
  , Monitor()
  ) where

import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Base

import Ivory.Language

handler :: (IvoryArea a)
        => ChanOutput a -> String -> Handler a () -> Monitor ()
handler (ChanOutput (Chan chanast)) name block = do
  runHandler name chanast block

state :: (IvoryArea a)
      => String -> Monitor (Ref Global a)
state n = do
  f <- freshname n
  let a = area (showUnique f) Nothing
  monitorPutCode $ \_ -> defMemArea a
  return (addrOf a)
