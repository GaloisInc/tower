
module Ivory.Tower.AST.Handler
  ( Handler(..)
  , emptyHandler
  , handlerInsertEmitter
  , handlerInsertCallback
  ) where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter

data Handler = Handler
  { handler_name      :: Unique
  , handler_chan      :: Chan
  , handler_emitters  :: [Emitter]
  , handler_callbacks :: [String]
  } deriving (Eq, Show)

emptyHandler :: Unique -> Chan -> Handler
emptyHandler u c = Handler
  { handler_name      = u
  , handler_chan      = c
  , handler_emitters  = []
  , handler_callbacks = []
  }

handlerInsertEmitter :: Emitter -> Handler -> Handler
handlerInsertEmitter a h =
  h { handler_emitters = a : (handler_emitters h) }

handlerInsertCallback :: String -> Handler -> Handler
handlerInsertCallback a h =
  h { handler_callbacks = a : (handler_callbacks h) }

