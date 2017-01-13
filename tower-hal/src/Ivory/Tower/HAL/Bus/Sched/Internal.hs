{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.HAL.Bus.Sched.Internal where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

data Task req res = Task
  { taskName :: String
  , taskReq :: ChanOutput req
  , taskRes :: ChanInput res
  }

-- | Make a task with the given name for later scheduling.
task :: (IvoryArea req, IvoryArea res)
     => String
     -> Tower e (Task req res, BackpressureTransmit req res)
task taskName = do
  (backpressureTransmit, taskReq) <- channel
  (taskRes, backpressureComplete) <- channel
  return (Task { .. }, BackpressureTransmit { .. })

data TaskState req res = TaskState
  { taskBase :: Task req res
  , taskId :: Uint32
  , taskPending :: Ref 'Global ('Stored IBool)
  , taskLastReq :: Ref 'Global req
  }
