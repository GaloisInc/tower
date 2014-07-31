{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower
  -- Tower monad:
  ( Tower
  -- | The Tower monad is the outer context for the Tower language.
  --   Phantom type 'p' is used to track platform constraints.
  , towerArtifact
  -- | Write an 'Artifact' to the Tower context. Compiler will output artifacts
  --   according to user options.
  , towerModule
  -- | Write an Ivory 'Module' to the tower context. Compiler will output these
  --   modules.
  , towerDepends
  -- | Provide a 
  , towerGroup
  -- | Tower tasks are arranged in a hierarchy of groups. These groups make no
  --   semantic difference in code generation, but may be used aid reporting
  --   tools.
  , channel
  -- | Create a channel with automatic queue depth of 16, and no initial value.
  , channel'
  -- | Create a channel of specified queue depth, with an optional initial
  --   value. The initial value is only accessable by a 'ChannelReader', it
  --   is not delivered as an 'Event' or pushed onto a 'ChannelReceiver' queue.

  --Task monad:
  , Task
  -- | The Task monad is the context for tasks in the Tower language. A task is
  --   a collection of mutually exclusive event handlers and their shared data.
  , task
  -- | Instantiate a 'Task' in a 'Tower' context. Takes a String parameter for
  --   human-readable reporting and debugging.
  , taskInit
  -- | Takes an Ivory callback that will be run exactly once. All 'taskInit'
  --   handlers will be run before any event handlers are run.
  , taskLocal
  -- | Creates a unique global memory area which, local to the context of the
  --   'Task'. All event handlers in the 'Task' may access this memory area
  --   safely. Assumes the user doesn't do anything evil, like share a Ref to
  --   this area to another Task via a channel.
  --   Takes a String parameter for human-readable reporting and
  --   debugging.
  , taskLocalInit
  -- | Like 'taskLocal' but with an initial value.
  , taskModuleDef
  -- | User provides arbitrary ModuleDef to be added to the generated code
  --   module. This is a good place to 'incl' helper procedures. For memory
  --   areas, prefer to use 'taskLocal'.
  , taskChannel
  -- | 'channel' lifted into the context of 'Task'.
  , taskChannel'
  -- | 'channel'' lifted into the context of 'Task'.
  , taskPriority
  -- | Provide a numeric priority for the task. Must be greater than zero.
  --   Higher integers for higher priorities.
  --   Tasks with the same priority may be ordered arbitrarily on operating
  --   systems which require unique priority levels.
  , taskStackSize
  -- | Require a minimum stack size for the task, in bytes. All tasks have a
  --   default stack size given by the OS backend.


  -- Channel API
  , ChannelSource
  -- | The writable end of a channel, created in the 'Tower' context.
  , ChannelSink
  -- | The readable end of a channel, created in the 'Tower' context.
  , ChannelEmitter
  -- | A writable channel, brought into the scope of a 'Task'. ChannelEmitter
  --   posts messages to a channel with a noblocking write 'Ivory' statement.
  , ChannelReceiver
  -- | A receivable channel, brought into the scope of a 'Task'.
  --   ChannelReceiver pops messages from the channel queue (FIFO) with a
  --   nonblocking read 'Ivory' statement.
  , ChannelReader
  -- | A readable channel, brought into the scope of a 'Task'. ChannelReader
  --   gets the most recent message written to a channel with a noblocking read
  --   'Ivory' statement.
  , src
  -- | convenience accessors for the common source/sink pair given by 'channel'
  , snk
  -- | convenience accessors for the common source/sink pair given by 'channel'

  , withChannelEmitter
  -- | Transforms a 'ChannelSource' to a 'ChannelEmitter' in the context of a
  --   'Task'. Takes a String parameter for human-readable reporting and
  --   debugging.
  , emit_
  -- | Ivory statement writes a message to channel brought into scope as a 
  --   'ChannelEmitter'.
  , emitV_
  -- | like 'emit_' but takes a value ('Stored' Ivory val) rather than a
  --   reference.

  , withChannelReceiver
  -- | Transforms a 'ChannelSink' to a 'ChannelReceiver' in the context of a
  --   'Task'. Takes a String parameter for human-readable reporting and
  --   debugging.
  , receive
  -- | Pop a message from a 'ChannelReceiver' queue if it is available. Does not
  --   block. Returns 'true' when a message has been popped.
  , receiveV
  -- | Like 'receive' but gives a value ('Stored' Ivory val) rather than a
  --   reference.
  , withChannelReader
  -- | Transforms a 'ChannelSink' to a 'ChannelReader' in the context of a
  --   'Task'. Takes a String parameter for human-readable reporting and
  --   debugging.
  , chanRead
  -- | Read the latest message from a 'ChannelReader'. Only returns false
  --   when no initial value was given (see 'channel'') and no message has
  --   been posted to the channel since the system was initialized. Does not
  --   indicate if the message has changed since the last read.
  , chanReadV
  -- | Like 'chanRead' but gives a value ('Stored' Ivory val) rather than a
  --   reference.

  -- Event API:
  , withChannelEvent
  -- | Transforms a 'ChannelSink area' into an 'Event area' in the context
  --   of a task. The resulting 'Event' will deliver each message posted to
  --   the channel queue (FIFO).

  , Event
  -- | A message which may handled in the task's event loop.
  , handle
  -- | Register a handler for a given 'Event'. Many handlers can be registered
  --   for a given Event - all will be run for each message.
  --   Takes a String parameter for human-readable reporting and debugging.
  , handleV
  -- | Like 'handle' but the callback is passed a value ('Stored' Ivory val)
  --   rather than a reference.


  -- Time API:
  , module Ivory.Tower.Types.Time
  , getTime
  -- | An Ivory statement that gives the current system time ('ITime').
  , withPeriodicEvent
  -- | Create a periodic event in the context of a 'Task'. Take a period
  --   specified as a constant 'Time', and gives an 'Event'. A message
  --   containing the current 'ITime' will be delivered on the specified
  --   period.

  -- Signal API:
  , Signalable(..)
  -- | Class describing standard methods and associated 'SignalType'.
  , NoSignals
  -- | Utility class
  , withSignalEvent
  -- | Create a signal event in the context of a 'Task'.
  --   'Event' will notify any handlers that the external event has occured.
  --   The message can contain no information, the 'IBool' can be considered
  --   a placeholder (will always be 'true').

  -- Artifact API:
  , Artifact(..)
  -- | Compiler can create an arbitrary file at code generation time.

  -- Naming utilities:
  , Unique
  -- | A generated name
  , BaseUtils(fresh, freshname)
  -- | Utilities for generating 'Unique' names.
  , showUnique
  -- | Safely a 'Unique' into a string.

  -- Utility functions for legacy support:
  , onPeriod
  -- | Utility function for legacy support - creates a periodic event and
  --   specifieds a handler.
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
         -> (forall s . ITime -> Ivory (AllocEffects s) ())
         -> Task p ()
onPeriod per k = do
  evt <- withPeriodicEvent per
  taskEventHandler evt "periodic" $ do
    handleV k


