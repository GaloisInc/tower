module Main where

import Ivory.Tower.Test.FooBarSignals
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf fooBarTower

