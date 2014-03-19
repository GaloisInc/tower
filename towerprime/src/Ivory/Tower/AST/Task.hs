{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ivory.Tower.AST.Task
  ( Task(..)
  ) where

import Ivory.Tower.Types.Signalable
import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.ChanEmitter
import Ivory.Tower.AST.ChanReceiver
import Ivory.Tower.AST.Event
import Ivory.Tower.AST.EventHandler
import Ivory.Tower.AST.SignalReceiver

data Task p =
  Task
    { task_name                 :: Unique
    , task_chan_emitters        :: [ChanEmitter]
    , task_chan_poll_receivers  :: [ChanReceiver]
    , task_chan_event_receivers :: [ChanReceiver]
    , task_signal_receivers     :: [SignalReceiver (SignalType p)]
    , task_evts                 :: [Event]
    , task_evt_handlers         :: [EventHandler]
    , task_priority             :: Integer
    }

deriving instance (Show (SignalType p)) => Show (Task p)
deriving instance (Eq   (SignalType p)) => Eq   (Task p)
