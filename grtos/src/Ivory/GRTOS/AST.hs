{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.AST where

import Ivory.Language

-- AST

data Callback = Callback (forall eff . Ivory eff ())

-- Bogus instances just to get the rest to work
instance Eq Callback where
  _ == _ = True
instance Show Callback where
  show _ = "Callback"

data Event =
  Event
    { event_num :: Int
    , event_name :: String
    } deriving (Eq, Show)

data Priority =
  Priority
    { pri_level :: Int
    , pri_events :: [(Event, Callback)]
    } deriving (Eq, Show)

data System = System [Priority]

