{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ivory.Tower.AST.Handler
  ( Handler(..)
  ) where

import Ivory.Tower.AST.Trigger
import Ivory.Tower.AST.ChanEmitter
import Ivory.Tower.Types.Unique

data Handler =
  Handler
    { handler_name       :: Unique
    , handler_annotation :: String
    , handler_trigger    :: Trigger
    , handler_emitters   :: [ChanEmitter]
    } deriving (Eq, Show)

