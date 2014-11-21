--
-- Top-level driver for AADL generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL
  ( compileAADL
  ) where

import Prelude hiding (log)

import Tower.AADL.Monad

import Ivory.Tower
import qualified Ivory.Tower.AST as A
import qualified Ivory.Tower.Types.GeneratedCode as T
import qualified Ivory.Tower.Types.ThreadCode as T

import Ivory.Language (package, Module)
import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Syntax.Type as I

import Data.List (find, nubBy, isPrefixOf)
import qualified Data.Map as Map

import Text.Show.Pretty -- XXX testing

--------------------------------------------------------------------------------

data VoidPlatform = VoidPlatform

compileAADL :: Tower VoidPlatform () -> IO ()
compileAADL t = do
  -- args <- getArgs
  -- opts <- parseOpts args

  let (towerAST, genCode) = runTower t VoidPlatform
  let chanTypes = mkChanMap towerAST (threadModules genCode)

  let m = fromTower towerAST
  let (res, log) = runGenM (Config chanTypes) m
  putStrLn $ "Log: " ++ show log
  putStrLn (ppShow res)

fromTower :: A.Tower -> GenM ()
fromTower t = undefined

-- | Return the Ivory generated code modules for a particular Tower.
threadModules :: GeneratedCode -> [Module]
threadModules genCode = map modThd threads
  where
  threads = Map.assocs (T.generatedcode_threads genCode)
  modThd (thread, code) = package "" (T.threadcode_gen code)

-- | For each chan, construct it's name and look up the emitter area in the
-- generated code to find it's type.
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

