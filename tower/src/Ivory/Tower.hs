{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower
  -- Channel API:
  ( ChannelSource
  , ChannelSink
  , ChannelEmitter
  , ChannelReceiver
  , ChannelReader
  , src
  , snk
  , withChannelEmitter
  , withChannelReceiver
  , emit_
  , emitV_
  , receive
  , receiveV
  , withChannelEvent
  , withChannelReader
  , chanRead
  , chanReadV

  -- Task API:
  , Task
  , task
  , taskInit
  , taskLocal
  , taskLocalInit
  , taskModuleDef
  , taskChannel
  , taskChannel'
  , taskPriority
  , taskStackSize

  -- Tower API:
  , Tower
  , towerArtifact
  , towerModule
  , towerDepends
  , towerGroup
  , channel
  , channel'

  -- Event API:
  , Event
  , handle
  , handleV

  -- Time API:
  , module Ivory.Tower.Types.Time
  , getTime
  , timerEvent

  -- Signal API:
  , Signalable(..)
  , NoSignals
  , withSignalEvent

  -- Artifact API:
  , Artifact(..)

  -- Naming utilities:
  , Unique
  , BaseUtils(fresh, freshname)
  , showUnique

  -- Utility functions for legacy support:
  , onPeriod
  ) where

import Ivory.Tower.Types.Channels
import Ivory.Tower.Channel

import Ivory.Tower.Task

import Ivory.Tower.Tower

import Ivory.Tower.Event

import Ivory.Tower.Types.Time
import Ivory.Tower.Timer

import Ivory.Tower.Types.Signalable
import Ivory.Tower.Signal

import Ivory.Tower.Types.Artifact

import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Unique

import Ivory.Language

onPeriod :: (Time a)
         => a
         -> (forall s . ITime -> Ivory (ProcEffects s ()) ())
         -> Task p ()
onPeriod per k = do
  evt <- timerEvent per
  handleV evt "periodic" k

