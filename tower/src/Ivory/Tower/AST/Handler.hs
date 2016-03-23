module Ivory.Tower.AST.Handler
  ( Handler(..)
  , handlerName
  ) where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter
import Ivory.Tower.AST.Comment
import Ivory.Tower.Types.Opts

import qualified Ivory.Language.Syntax.AST as AST

data Handler = Handler
  { handler_name         :: Unique
  , handler_chan         :: Chan
  , handler_emitters     :: [Emitter]
  , handler_callbacks    :: [Unique]
  , handler_callbacksAST :: [AST.Proc]
  , handler_comments     :: [Comment]
  , handler_transformers :: [Opt]
  } deriving (Show, Eq, Ord)

handlerName :: Handler -> String
handlerName = showUnique . handler_name
