{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Language
import Ivory.Tower

import Text.Show.Pretty
import Tower.AADL.Render
import Tower.AADL.FromTower

import Text.PrettyPrint.Leijen

type State = Stored Sint8

test1_per :: Tower e ()
test1_per = do
  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \m -> emit e (constRef (s :: Ref Global State))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

config = Config { .. }
  where
  configThreadModules = ["thread_module"]
  configSystemName = "sys"
  configSystemOS = "OS"
  configSystemHW = "HW"

main :: IO ()
main = do
  let (towerAST, genCode) = runTower test1_per undefined
  putStrLn (ppShow towerAST)
  let ast = fromTower config towerAST
  let doc = renderSystem ast
  putDoc doc
