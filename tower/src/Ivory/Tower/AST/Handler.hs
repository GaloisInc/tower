{-# LANGUAGE CPP         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.Handler
  ( Handler(..)
  , handlerName
  ) where

import Data.Monoid ((<>))
#if MIN_VERSION_mainland_pretty(0,6,0)
import           Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

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

handlerName :: Handler -> String
handlerName = showUnique . handler_name

instance Pretty Handler where
  ppr h@(Handler{..}) = hang 2 $
        text (handlerName h) <> colon
    </> "Chan:" <+/> ppr handler_chan
    </> "Emitters:" <+/> pprList handler_emitters
    </> "Callbacks:" <+/> pprList (map showUnique handler_callbacks)
    </> "Comments:" <+/> pprList handler_comments
