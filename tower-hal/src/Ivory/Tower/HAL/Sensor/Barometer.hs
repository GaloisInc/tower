{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Sensor.Barometer where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

[ivory|

struct barometer_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; pressure    :: Stored IFloat -- mbar
  ; temperature :: Stored IFloat -- deg celsius
  ; time        :: Stored ITime
  }
|]

barometerTypesModule :: Module
barometerTypesModule = package "barometer_types" $ do
  defStruct (Proxy :: Proxy "barometer_sample")
  depend serializeModule
  wrappedPackMod barometerWrapper

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

barometerWrapper :: WrappedPackRep (Struct "barometer_sample")
barometerWrapper = wrapPackRep "barometer_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel pressure
  , packLabel temperature
  , packLabel' time packITime
  ]

instance Packable (Struct "barometer_sample") where
  packRep = wrappedPackRep barometerWrapper
