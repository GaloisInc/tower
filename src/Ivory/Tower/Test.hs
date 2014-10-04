
module Ivory.Tower.Test where

import Ivory.Tower
import Ivory.Tower.ToyObjLang
import Ivory.Tower.AST.Graph
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

test2 :: Tower ()
test2 = do
  (c1in, c1out) <- channel
  (c2in, c2out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    handler per "tick" $ do
      emitter c1in 1
      callback "some_ivory_in_m1_tick"
  monitor "m2" $ do
    handler c1out "chan1msg" $ do
      callback "some_ivory_in_m2_onmsg"
      emitter c2in 1
  monitor "m3" $ do
    handler c1out "chan1msg" $ do
      callback "some_ivory_in_m3_onchan1"
      emitter c2in 1
    handler c2out "chan2msg" $ do
      callback "some_ivory_in_m3_onchan2"
  monitor "m4" $ do
    handler c2out "chan2msg" $ do
      callback "some_ivory_in_m4_onmsg"

run :: Tower () -> IO ()
run t = do
  putStrLn (ppShow ast)
  putStrLn "\n=======\n"
  let graph = messageGraph ast
      dot = graphviz graph
  putStrLn dot
  writeFile "out.dot" dot
  putStrLn "\n=======\n"
  printModules (generatedCodeModules code)
  where
  (ast, code) = tower t

