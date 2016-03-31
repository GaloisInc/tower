module Ivory.Tower.AST.Handler
  ( Handler(..)
  , handlerName
  ) where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter
import Ivory.Tower.AST.Comment
import Ivory.Tower.Types.Opts
import Data.List.NonEmpty
--import Ivory.Tower.Types.Backend

import qualified Ivory.Language.Syntax.AST as AST

data Handler = Handler
  { handler_name         :: Unique
  , handler_chan         :: Chan
  , handler_emitters     :: [Emitter]
  , handler_callbacks    :: NonEmpty Unique      -- a monitor should have at least 1 callback
  , handler_callbacksAST :: NonEmpty AST.Proc    -- a monitor should have at least 1 callback
  , handler_comments     :: [Comment]
  , handler_transformers :: [Opt]
  } deriving (Show, Eq)

instance Ord Handler where
  compare a b = compare 
    (handler_name a, handler_chan a, handler_emitters a, handler_callbacks a, handler_comments a, handler_transformers a) 
    (handler_name b, handler_chan b, handler_emitters b, handler_callbacks b, handler_comments b, handler_transformers b) 

handlerName :: Handler -> String
handlerName = showUnique . handler_name
