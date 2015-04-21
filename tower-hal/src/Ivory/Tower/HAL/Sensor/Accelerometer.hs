{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Sensor.Accelerometer where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

accelerometerTypesModule :: Module
accelerometerTypesModule = package "accelerometer_types" $ do
  defStruct (Proxy :: Proxy "accelerometer_sample")
  depend serializeModule
  wrappedPackMod accelerometerWrapper

[ivory|
struct accelerometer_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- m/s/s
  ; temp       :: Stored IFloat -- degrees Celsius
  ; time       :: Stored ITime
  }
|]

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

accelerometerWrapper :: WrappedPackRep (Struct "accelerometer_sample")
accelerometerWrapper = wrapPackRep "accelerometer_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel sample
  , packLabel temp
  , packLabel' time packITime
  ]

instance Packable (Struct "accelerometer_sample") where
  packRep = wrappedPackRep accelerometerWrapper
