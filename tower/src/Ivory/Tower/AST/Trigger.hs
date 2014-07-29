{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ivory.Tower.AST.Trigger
  ( Trigger(..)
  , triggerName
  ) where

import Ivory.Tower.AST.Event
import Ivory.Tower.AST.Chan
import Ivory.Tower.Types.Time

data Trigger
  = SynchronousTrigger  Chan
  | AsynchronousTrigger Event Microseconds -- bounding period
  deriving (Eq, Show)

triggerName :: Trigger -> String
triggerName (SynchronousTrigger c) = "sync_chan_" ++ (show (chan_id c))
triggerName (AsynchronousTrigger e _) = "async_" ++ (eventName e)


