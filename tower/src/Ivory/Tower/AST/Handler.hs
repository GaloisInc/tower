
module Ivory.Tower.AST.Handler
  ( Handler(..)
  , emptyHandler
  , handlerName
  , handlerInsertEmitter
  , handlerInsertCallback
  , handlerInsertComment
  ) where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter
import Ivory.Tower.AST.Comment

data Handler = Handler
  { handler_name      :: Unique
  , handler_chan      :: Chan
  , handler_emitters  :: [Emitter]
  , handler_callbacks :: [Unique]
  , handler_comments  :: [Comment]
  } deriving (Eq, Show, Ord)

emptyHandler :: Unique -> Chan -> Handler
emptyHandler u c = Handler
  { handler_name      = u
  , handler_chan      = c
  , handler_emitters  = []
  , handler_callbacks = []
  , handler_comments  = []
  }

handlerName :: Handler -> String
handlerName = showUnique . handler_name

handlerInsertEmitter :: Emitter -> Handler -> Handler
handlerInsertEmitter a h =
  h { handler_emitters = handler_emitters h ++ [a] }

handlerInsertCallback :: Unique -> Handler -> Handler
handlerInsertCallback a h =
  h { handler_callbacks = handler_callbacks h ++ [a] }

handlerInsertComment :: Comment -> Handler -> Handler
handlerInsertComment c h =
  h { handler_comments = c : (handler_comments h) }

