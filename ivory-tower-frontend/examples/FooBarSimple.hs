module Main where

import Ivory.Tower.Test.FooBarSimple
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf fooBarTower

