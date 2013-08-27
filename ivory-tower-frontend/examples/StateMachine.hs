module Main where

import Ivory.Tower.Test.StateMachine
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf stateMachineTestTower

