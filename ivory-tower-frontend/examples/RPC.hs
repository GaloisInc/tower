module Main where

import Ivory.Tower.Test.RPC
import Ivory.Tower.Frontend

main :: IO ()
main = compile defaultBuildConf rpcTower

