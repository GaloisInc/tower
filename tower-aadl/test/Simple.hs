{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Language
import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Syntax.AST  as I
import Ivory.Tower

import Data.List

import Ivory.Tower.Compile

import Ivory.Tower.AST.Graph

import qualified Data.Map as Map
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import qualified Ivory.Tower.Types.Unique as U
import qualified Ivory.Tower.AST as A

import qualified Ivory.Compile.C.CmdlineFrontend as C

import Text.Show.Pretty

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

main :: IO ()
main = do
  let (towerAST, genCode) = runTower test1_per undefined
  putStrLn (ppShow towerAST)

  let monitors = Map.assocs (generatedcode_monitors genCode)
  let modMon (monitor, code) =
       package (showUnique (A.monitor_name monitor))
               (monitorcode_moddef code)
  let monitorMods = map modMon monitors

  let threads  = Map.assocs (generatedcode_threads genCode)
  let modThd (thread, code) =
       package ("thread")
               (threadcode_gen code)
  let threadMods  = map modThd threads

  -- putStrLn (ppShow threadMods)
  -- putStrLn (ppShow (mkChanMap towerAST threadMods))
  let graph = messageGraph towerAST
  putStrLn (ppShow $ map (threadHandlers graph) (A.towerThreads towerAST))


type ChanTypes = Map.Map A.Chan I.Type

-- | For a representative emitter for a chan, construct it's name and look up
-- the emitter area in the generated code to find it's type.
mkChanMap :: A.Tower -> [Module] -> ChanTypes
mkChanMap t m = Map.fromList (map go eSet)
  where
  handlers = concatMap A.monitor_handlers (A.tower_monitors t)
  emitters = concatMap A.handler_emitters handlers

  eSet :: [A.Emitter]
  eSet = nubBy (\e e' -> A.emitter_chan e == A.emitter_chan e') emitters

  areas :: [I.Area]
  areas = concatMap ((\(I.Visible pub _priv) -> pub) . I.modAreas) m

  areaTypes :: [(String, I.Type)]
  areaTypes = map (\area -> (I.areaSym area, I.areaType area)) areas

  go :: A.Emitter -> (A.Chan, I.Type)
  go e = let c = A.emitter_chan e in
         let n = showUnique (A.emitter_name e) in
         case find (isPrefixOf n . fst) areaTypes of
           Nothing
             -> error $ "Impossilbe error in mkChanMap in Tower->AADL: "
                     ++ "couldn't find type for emitter " ++ show n
           Just (_s,t)
             -> (c, t)
