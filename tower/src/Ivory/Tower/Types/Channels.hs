{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Types.Channels
  ( ChannelSource(..)
  , ChannelSink(..)
  , ChannelEmitter(..)
  , ChannelReceiver(..)
  , ChannelReader(..)
  ) where

import Ivory.Language
import Ivory.Tower.AST.Chan

newtype ChannelSource (area :: Area *) =
  ChannelSource { unChannelSource :: Chan }

newtype ChannelSink (area :: Area *) =
  ChannelSink { unChannelSink :: Chan }

newtype ChannelEmitter (area :: Area *) =
  ChannelEmitter
    { unChannelEmitter :: forall eff s . ConstRef s area -> Ivory eff () }

newtype ChannelReceiver (area :: Area *) =
  ChannelReceiver
    { unChannelReceiver :: forall eff s . Ref s area -> Ivory eff IBool }

newtype ChannelReader (area :: Area *) =
  ChannelReader
    { unChannelReader :: forall eff s . Ref s area -> Ivory eff IBool }
