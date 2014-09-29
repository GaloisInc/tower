
module Ivory.Tower.Test where

import Ivory.Tower.Tower

test1 :: Tower ()
test1 = do
  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    handler per "tick" $ do
      emitter c1in 1
      callback "some_ivory_in_m1_tick"
  monitor "m2" $ do
    handler c1out "chan1msg" $ do
      callback "some_ivory_in_m2_onmsg"


