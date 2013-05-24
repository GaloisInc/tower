module Main where

import qualified Ivory.Compile.C.CmdlineFrontend as C

--import Ivory.Tower.Graphviz
import Ivory.Tower.Test.FooBarTower
import qualified Ivory.Tower.Compile.FreeRTOS as FreeRTOS

main :: IO ()
main = do
  let (asm, objs) = FreeRTOS.compile fooBarTower
  C.compile objs
--  graphvizToFile "out.dot" asm

