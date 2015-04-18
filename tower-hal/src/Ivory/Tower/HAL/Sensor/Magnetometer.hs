{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Sensor.Magnetometer where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

magnetometerTypesModule :: Module
magnetometerTypesModule = package "magnetometer_types" $ do
  defStruct (Proxy :: Proxy "magnetometer_sample")
  depend serializeModule
  wrappedPackMod magnetometerWrapper

[ivory|
struct magnetometer_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- Gauss
  ; time       :: Stored ITime
  }
|]

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

magnetometerWrapper :: WrappedPackRep (Struct "magnetometer_sample")
magnetometerWrapper = wrapPackRep "magnetometer_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel sample
  , packLabel' time packITime
  ]

instance Packable (Struct "magnetometer_sample") where
  packRep = wrappedPackRep magnetometerWrapper
