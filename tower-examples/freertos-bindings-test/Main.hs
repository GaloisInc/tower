
module Main where

import Ivory.OS.FreeRTOS.Artifacts
import Ivory.Compile.C.CmdlineFrontend


main :: IO ()
main = compile [] as
  where
  as = kernel config ++ wrappers
  config = Config
    { cpu_clock_hz = 168 * 1000 * 1000
    , tick_rate_hz = 1000
    , max_priorities = 12
    , minimal_stack_size = 512
    , total_heap_size = 64 * 1024 * 1024
    }

