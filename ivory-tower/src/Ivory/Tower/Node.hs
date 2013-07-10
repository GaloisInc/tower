{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Node where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Monad

-- | private
codegenChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
                       => ChannelReceiver n area -> Node i ()
codegenChannelReceiver rxer = do
  os <- getOS
  thisnode <- getNode
  let (channelinit, mdef) = os_mkChannel os rxer thisnode
  nodeStAddCodegen channelinit mdef

class ChannelEmittable i where
  -- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
  --   'Task'.
  --   Provide a human-readable name as a debugging aid.
  withChannelEmitter :: (SingI n, IvoryArea area)
        => ChannelSource n area -> String -> Node i (ChannelEmitter n area)

-- | Transform a 'DataSink' into a 'DataReader' in the context of a
--   'Task'. Provide a human-readable name as a debugging aid.
withDataReader :: (IvoryArea area)
               => DataSink area -> String -> Node i (DataReader area)
withDataReader ds label = do
  let dpid = unDataSink ds
  nodeStAddDataReader dpid label
  return (DataReader dpid)

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Task '. Provide a human-readable name as a debugging aid.
withDataWriter :: (IvoryArea area)
               => DataSource area -> String -> Node i (DataWriter area)
withDataWriter ds label = do
  let dpid = unDataSource ds
  nodeStAddDataWriter dpid label
  return (DataWriter dpid)

-- | Transform a 'ChannelSink' into a 'ChannelReceiver' in the context of a
--   'Task'.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
      => ChannelSink n area -> String -> Node i (ChannelReceiver n area)
withChannelReceiver chsink label = do
  let cid  = unChannelSink chsink
      rxer = toReceiver chsink
  -- Register the receiver into the graph context
  nodeStAddReceiver cid label
  -- Generate code implementing the channel for this receiver.
  codegenChannelReceiver rxer
  return rxer
  where
  toReceiver :: ChannelSink n area -> ChannelReceiver n area
  toReceiver sink = ChannelReceiver $ unChannelSink sink

