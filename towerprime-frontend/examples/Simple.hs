module Main where

import Ivory.Tower.Test.TaskTest
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf tower_simple_per_tasks

