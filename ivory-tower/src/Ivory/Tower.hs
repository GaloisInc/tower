
module Ivory.Tower
  -- Channel exports:
  ( ChannelSource()
  , ChannelSink()
  , ChannelEmitter()
  , ScheduledReceiver()

  , emit

  -- DataPort exports:
  , DataPort()
  , DataSource()
  , DataSink()
  , DataReader()
  , DataWriter()

  , readData
  , writeData

  -- Monad exports:
  , getTimeMillis
  , Tower()
  , Task()
  , Scheduled()
  , TaskConstructor

  , OSGetTimeMillis()

  , freshname
  -- Tower exports:
  , task
  , dataport
  , channel
  , addModule

  , withContext
  , withPeriod
  , withGetTimeMillis

  , withChannelEmitter
  , withChannelReceiver

  , withDataReader
  , withDataWriter

  , taskLoop

  -- Task exports:
  , withStackSize
  , withPriority
  , withModule

  , taskModuleDef

  -- EventLoop exports:
  , onChannel
  , onTimer
  , handlers

  -- Types
  , Name
  , Assembly()
  ) where

import Ivory.Tower.Channel
import Ivory.Tower.DataPort
import Ivory.Tower.EventLoop
import Ivory.Tower.Monad
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Types

