{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ivory.Tower.AST.System
  ( System(..)
  , system_task_list
  , event_emitters
  , poll_receivers
  , event_receivers
  ) where

import Ivory.Tower.AST.Chan
import qualified Ivory.Tower.AST.Directory as D
import Ivory.Tower.AST.Task
import Ivory.Tower.AST.ChanEmitter
import Ivory.Tower.AST.ChanReceiver
import Ivory.Tower.Types.Unique

data System p =
  System
    { system_channels :: [Chan]
    , system_tasks    :: D.Dir Unique (Task p)
    }

deriving instance (Show (Task p)) => Show (System p)
deriving instance (Eq   (Task p)) => Eq   (System p)

system_task_list :: System p -> [Task p]
system_task_list sys = map snd (D.flatten (system_tasks sys))

event_emitters :: System p -> Chan -> [(ChanEmitter, Task p)]
event_emitters sys chan =
    concat (map (\t -> zip (matchingemitters t) (repeat t)) ts)
    where
    matchingemitters t = filter p (task_chan_emitters t)
    p ce = chanemitter_chan ce == chan
    ts = map snd (D.flatten (system_tasks sys))

poll_receivers :: System p -> Chan -> [(ChanReceiver, Task p)]
poll_receivers sys chan =
    concat (map (\t -> zip (matchingrxers t) (repeat t)) ts)
    where
    matchingrxers t = filter p (task_chan_poll_receivers t)
    p cr = chanreceiver_chan cr == chan
    ts = map snd (D.flatten (system_tasks sys))

event_receivers :: System p -> Chan -> [(ChanReceiver, Task p)]
event_receivers sys chan =
    concat (map (\t -> zip (matchingrxers t) (repeat t)) ts)
    where
    matchingrxers t = filter p (task_chan_event_receivers t)
    p cr = chanreceiver_chan cr == chan
    ts = map snd (D.flatten (system_tasks sys))

