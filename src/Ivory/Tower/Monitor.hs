
module Ivory.Tower.Monitor
  ( handler
  , Handler()
  , Monitor()
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor

handler :: ChanOutput a -> String -> Handler () -> Monitor ()
handler (ChanOutput (Chan chanast)) name block = do
  ast <- runHandler name chanast block
  monitorPutASTHandler ast

