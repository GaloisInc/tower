{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Types.Signalable
  ( Signalable(..)
  , NoSignals(..)
  ) where

class Signalable p where
  data SignalType p
  signals         :: [SignalType p]
  -- each value of SignalType p must be transferable to/from strings without
  -- losing information.
  signalName      :: SignalType p -> String
  -- signalFromName is partial, user is burdened with only giving valid names
  -- (i.e. those produced by signalName)
  signalFromName  :: String -> SignalType p

-- Utility class
data NoSignals = NoSignals
instance Signalable NoSignals where
  data SignalType NoSignals = NoSignal deriving (Show)
  signals = [NoSignal]
  signalName = show
  signalFromName = const NoSignal

