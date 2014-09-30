
module Ivory.Tower.Test where

import Ivory.Tower
import Ivory.Tower.Types.TowerCode
import Ivory.Tower.ToyObjLang
import Text.Show.Pretty

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

run :: Tower () -> IO ()
run t = do
  putStrLn (ppShow ast)
  printModules (towercode_modules code)
  where
  (ast, code) = tower t

