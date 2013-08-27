module Main where

import Ivory.Tower.Test.Sequential
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf sequentialTestTower

