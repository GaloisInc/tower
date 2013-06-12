{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.DataPort where

import Ivory.Language

import Ivory.Tower.Types

-- | Atomic read of shared data, copying to local reference. Always succeeds.
--   Takes a 'DataReader'.
readData :: (eff `AllocsIn` cs, IvoryArea area)
         => Schedule -> DataReader area -> Ref s area -> Ivory eff ()
readData sch reader ref = sch_mkDataReader sch reader ref

-- | Atomic write to shared data, copying from local reference. Always
--   succeeds. Takes a 'DataWriter'.
writeData :: (eff `AllocsIn` cs, IvoryArea area)
          => Schedule -> DataWriter area -> ConstRef s area -> Ivory eff ()
writeData sch writer ref = sch_mkDataWriter sch writer ref

