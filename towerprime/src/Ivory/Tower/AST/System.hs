
module Ivory.Tower.AST.System
  ( System(..)
  , tasks_emitting
  , tasks_receiving
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


tasks_emitting :: System -> Chan -> [Task]
tasks_emitting sys chan = filter hasemitter ts
  where
  hasemitter t = or (map (\ce -> chanemitter_chan ce == chan)
                      (task_chan_emitters t))
  ts = map snd (D.flatten (system_tasks sys))

tasks_receiving :: System -> Chan -> [Task]
tasks_receiving sys chan = filter hasreceiver ts
  where
  hasreceiver t = or (map (\ce -> chanreceiver_chan ce == chan)
                      (task_chan_receivers t))
  ts = map snd (D.flatten (system_tasks sys))

