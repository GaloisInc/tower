{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.Codegen
import qualified Ivory.Compile.C.CmdlineFrontend as C

test1_per :: Tower p ()
test1_per = do
  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    (_s :: Ref Global (Stored IBool)) <- state "some_m1_state"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \m -> do
        comment "some_ivory_in_m1_tick"
        emit e m
  monitor "m2" $ do
    (_s :: Ref Global (Stored IBool))<- state "some_m2_state"
    handler c1out "chan1msg" $ do
      callback $ \_ -> comment "some_ivory_in_m2_onmsg"

main :: IO ()
main = runTowerCompile test1_per platform copts
  where
  copts = C.initialOpts { C.outDir = Just "tower-examples-simple" }
  platform = stm32f4FreeRTOS
