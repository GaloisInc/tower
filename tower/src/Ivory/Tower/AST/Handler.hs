module Ivory.Tower.AST.Handler where

import Ivory.Tower.Types.Unique

import Ivory.Tower.AST.Chan
import Ivory.Tower.AST.Emitter
import Ivory.Tower.AST.Comment
import Ivory.Tower.Types.Opts
import Data.List.NonEmpty hiding (map)
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
  } deriving (Show, Eq, Ord)

handlerName :: Handler -> String
handlerName = showUnique . handler_name

data HandlerFast = HandlerFast
  { handler :: Handler}

instance Eq HandlerFast where
  (==) arg1 arg2 = 
    let a = handler arg1 in 
    let b = handler arg2 in 
    (==) 
      (handler_name a, map FastOpt $ handler_transformers a) 
      (handler_name b, map FastOpt $ handler_transformers b)

instance Ord HandlerFast where
  compare arg1 arg2 = 
    let a = handler arg1 in
    let b = handler arg2 in
    compare 
      (handler_name a, map FastOpt $ handler_transformers a) 
      (handler_name b, map FastOpt $ handler_transformers b) 

instance Show HandlerFast where
  show arg1 =
    let a = handler arg1 in
    show (handler_name a, map FastOpt $ handler_transformers a)
