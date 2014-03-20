{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  , NoSignals(..)
  ) where

import Ivory.Language (Proxy)

class Signalable p where
  data SignalType p
  signals         :: [SignalType p]
  -- each value of SignalType p must be transferable to/from strings without
  -- losing information.
  signalName      :: SignalType p -> String
  -- signalFromName is partial, user is burdened with only giving valid names
  -- (i.e. those produced by signalName)
  signalFromName  :: String -> SignalType p
  -- For code generation, external sources, headers, and searchdir which
  -- implement the signal (vector tables, etc)
  signalSources   :: Proxy p -> [FilePath]
  signalHeaders   :: Proxy p -> [FilePath]
  signalSearchDir :: Proxy p -> IO [FilePath]

-- Utility class XXX
data NoSignals = NoSignals
instance Signalable NoSignals where
  data SignalType NoSignals = NoSignal deriving (Show)
  signals = [NoSignal]
  signalFromName = const NoSignal
  signalName = show
  signalSources = const []
  signalHeaders = const []
  signalSearchDir = const (return [])

