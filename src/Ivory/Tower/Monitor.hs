
module Ivory.Tower.Monitor
  ( handler
  , state
  , Handler()
  , Monitor()
  ) where

import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Chan
import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Base

import Ivory.Tower.ToyObjLang

handler :: ChanOutput a -> String -> Handler () -> Monitor ()
handler (ChanOutput (Chan chanast)) name block = do
  (ast, code) <- runHandler name chanast block
  monitorPutASTHandler ast
  monitorPutThreadCode $ \twr -> generateHandlerCode code twr ast

state :: String -> Monitor Var
state n = do
  f <- freshname n
  let v = var (showUnique f)
  monitorPutCode $ \_ -> defVar v
  return v
