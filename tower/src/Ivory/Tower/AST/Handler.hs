
module Ivory.Tower.AST.Handler
  ( Handler(..)
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
