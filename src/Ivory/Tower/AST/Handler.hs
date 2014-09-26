
module Ivory.Tower.AST.Handler
  ( Handler(..)
  , emptyHandler
  , handlerInsertEmitter
  ) where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter

data Handler = Handler
  { handler_name     :: Unique
  , handler_chan     :: Chan
  , handler_emitters :: [Emitter]
  } deriving (Eq, Show)

emptyHandler :: Unique -> Chan -> Handler
emptyHandler u c = Handler
  { handler_name     = u
  , handler_chan     = c
  , handler_emitters = []
  }

handlerInsertEmitter :: Emitter -> Handler -> Handler
handlerInsertEmitter e h =
  h { handler_emitters = e : (handler_emitters h) }


