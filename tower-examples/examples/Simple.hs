{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Compile

tower_simple_test :: Tower p ()
tower_simple_test = return ()


main :: IO ()
main = undefined tower_simple_test

