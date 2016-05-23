{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module will go through a Handler and will 
-- return all the external ressources used by this Handler

module Ivory.Tower.Opts.LockCoarsening.Generate
  where

import Data.List (intersect)
import Data.Random
import Control.Concurrent.ParallelIO.Global

import Ivory.Language
import qualified Ivory.Tower.AST as AST
import qualified Data.List.NonEmpty as NE
import Ivory.Tower.Opts.LockCoarsening
import Ivory.Tower.Opts.LockCoarsening.LockOptimize

import qualified Ivory.Language.Area as I
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.Opts
--import Prelude


allpairs :: [t] -> [(t,t)]
allpairs [] = []
allpairs [_] = []
allpairs (x:xs) = concatMap (\y -> [(x,y)]) xs ++ allpairs xs

randomTest :: Int -> Int -> IO [[String]]
randomTest nbHandlers nbRessources = do
  let numberOfRessourcesPerHandler = [1..nbRessources]
  sequence $ flip map [1..nbHandlers] $ \_n -> flip runRVarT StdRandom $ (do 
    (nbRessourcesH) <- (randomElement numberOfRessourcesPerHandler)
    (shuffleNofM nbRessourcesH nbRessources $ map show [1..nbRessources]))

dummyHandler :: [String] -> Integer -> AST.Handler
dummyHandler s n = AST.Handler (Unique (show n) 1) (AST.ChanPeriod (AST.Period (us 0) perTy (us 0))) [] (NE.fromList []) (NE.fromList []) [] [(LockCoarsening $ OptHandler s)]
  where perTy = I.ivoryArea (Proxy :: I.AProxy ('Stored ITime))

runTest :: (Int, Int, Int) -> IO String
runTest (nbHandlers,nbRessources,nbLocks) = do
  ll <- (randomTest nbHandlers nbRessources)
  if (null $ filter (\(a,b) -> null $ intersect a b) (allpairs ll)) then
    return ""
  else do
    let list = zip ll $ map toInteger [1..nbHandlers]
    locks <- attributeLocksMonitor list nbLocks 60
    let optMon = (AST.Monitor (Unique (show (nbHandlers,nbRessources,nbLocks)) 1) (map (\(s, n) -> dummyHandler s n) list) AST.MonitorDefined mempty [(LockCoarsening $ OptMonitor locks)])
    (retMon,_numberAfterOpt) <- lockOptimizeMonitor optMon
    statisticsMonitor retMon

runTests :: [(Int, Int, Int)] -> IO ()
runTests params = do
  (testList::[String]) <- parallelInterleaved $ map runTest params
  sequence $ map putStrLn $ filter (not.null) testList
  stopGlobalPool