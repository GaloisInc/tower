
module Ivory.Tower.AST.System
  ( System(..)
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

data System =
  System
    { system_channels :: [Chan]
    , system_tasks    :: D.Dir Unique Task
    } deriving (Eq, Show)

event_emitters :: System -> Chan -> [(ChanEmitter, Task)]
event_emitters sys chan =
    concat (map (\t -> zip (matchingemitters t) (repeat t)) ts)
    where
    matchingemitters t = filter p (task_chan_emitters t)
    p ce = chanemitter_chan ce == chan
    ts = map snd (D.flatten (system_tasks sys))

poll_receivers :: System -> Chan -> [(ChanReceiver, Task)]
poll_receivers sys chan =
    concat (map (\t -> zip (matchingrxers t) (repeat t)) ts)
    where
    matchingrxers t = filter p (task_chan_poll_receivers t)
    p cr = chanreceiver_chan cr == chan
    ts = map snd (D.flatten (system_tasks sys))

event_receivers :: System -> Chan -> [(ChanReceiver, Task)]
event_receivers sys chan =
    concat (map (\t -> zip (matchingrxers t) (repeat t)) ts)
    where
    matchingrxers t = filter p (task_chan_event_receivers t)
    p cr = chanreceiver_chan cr == chan
    ts = map snd (D.flatten (system_tasks sys))

{- GARBAGE:
tasks_emitting :: System -> Chan -> [Task]
tasks_emitting sys chan = filter hasemitter ts
  where
  hasemitter t = or (map (\ce -> chanemitter_chan ce == chan)
                      (task_chan_emitters t))
  ts = map snd (D.flatten (system_tasks sys))

tasks_poll_receiving :: System -> Chan -> [Task]
tasks_poll_receiving sys chan = filter hasreceiver ts
  where
  hasreceiver t = or (map (\ce -> chanreceiver_chan ce == chan)
                      (task_chan_poll_receivers t))
  ts = map snd (D.flatten (system_tasks sys))

tasks_event_receiving :: System -> Chan -> [Task]
tasks_event_receiving sys chan = filter hasreceiver ts
  where
  hasreceiver t = or (map (\ce -> chanreceiver_chan ce == chan)
                      (task_chan_event_receivers t))
  ts = map snd (D.flatten (system_tasks sys))

-}
