{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.DataPort where

import Ivory.Language

import Ivory.Tower.Types

-- | Atomic read of shared data, copying to local reference. Always succeeds.
--   Takes a 'DataReader'.
readData :: (eff `AllocsIn` cs, IvoryType area)
         => DataReader area -> Ref s area -> Ivory eff ()
readData (DataReader dp) r = data_read dp r

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (eff `AllocsIn` cs, IvoryType area)
          => DataWriter area -> ConstRef s area -> Ivory eff ()
writeData (DataWriter dp) r = data_write dp r

