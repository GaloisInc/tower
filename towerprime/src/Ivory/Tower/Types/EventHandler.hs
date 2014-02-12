{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Types.EventHandler
  ( EventHandler(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Event
import Ivory.Tower.Types.Unique

data EventHandler
  = EventHandler
    { evthandler_name       :: Unique
    , evthandler_annotation :: String -- XXX required?
    , evthandler_evt        :: Event
    , evthandler_proc       :: Def('[]:->()) -- XXX?
    , evthandler_moddef     :: ModuleDef
    }

