{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.AST.EventHandler
  ( EventHandler(..)
  ) where

import Ivory.Tower.AST.Event
import Ivory.Tower.Types.Unique

data EventHandler
  = EventHandler
    { evthandler_name       :: Unique
    , evthandler_annotation :: String
    , evthandler_evt        :: Event
    }

