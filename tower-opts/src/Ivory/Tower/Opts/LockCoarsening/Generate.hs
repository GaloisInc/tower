{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module will go through a Handler and will 
-- return all the external ressources used by this Handler

module Ivory.Tower.Opts.LockCoarsening.Generate
  where

import Data.List (intersect)
import Data.Random
import System.Random
import Data.Monoid()
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

import Debug.Trace
import Data.Either (isLeft)
import Control.Exception.Enclosed

import Data.Random.Source
import Data.Random.Internal.Source
import Control.Monad.Loops

allpairs :: [t] -> [(t,t)]
allpairs [] = []
allpairs [_] = []
allpairs (x:xs) = concatMap (\y -> [(x,y)]) xs ++ allpairs xs


instance RandomSource IO StdGen where
    getRandomWord8From _ = getStdRandom random
    getRandomWord16From _ = getStdRandom random
    getRandomWord32From _ = getStdRandom random
    getRandomWord64From _ = getStdRandom random
    getRandomDoubleFrom _ = getStdRandom $ randomR (0,1)
    getRandomNByteIntegerFrom _ n = getStdRandom $ randomR (0,256^n-1)

randomTest :: Int -> Int -> StdGen -> IO [[String]]
randomTest nbHandlers nbRessources rr = do
  let numberOfRessourcesPerHandler = [1..nbRessources]
  sequence $ flip map [1..nbHandlers] $ \_n -> flip runRVarT rr $ (do 
    (nbRessourcesH) <- (randomElement numberOfRessourcesPerHandler)
    (shuffleNofM nbRessourcesH nbRessources $ map show [1..nbRessources]))



dummyHandler :: [String] -> Integer -> AST.Handler
dummyHandler s n = AST.Handler (Unique (show n) 1) (AST.ChanPeriod (AST.Period (us 0) perTy (us 0))) [] (NE.fromList []) (NE.fromList []) [] [(LockCoarsening $ OptHandler s)]
  where perTy = I.ivoryArea (Proxy :: I.AProxy ('Stored ITime))

selectTest :: (Int, Int, Int, Int) -> IO [[String]]
selectTest (nbHandlers, nbRessources, nbLocks, cputimelim) = do
  let rr = mkStdGen $ nbHandlers*150000+nbRessources*1000+nbLocks
  setStdGen rr
  iterateUntil (\x -> not.null $ filter (\(a,b) -> null $ intersect a b) (allpairs x)) $ do
    stdGen <- getStdGen
    setStdGen $ snd $ next $ stdGen
    testCase <- randomTest nbHandlers nbRessources stdGen
    pure testCase

runTest :: (Int, Int, Int, Int) -> IO String
runTest (nbHandlers, nbRessources, nbLocks, cputimelim) = do
  ll <- selectTest (nbHandlers, nbRessources, nbLocks, cputimelim)
  if (null $ filter (\(a,b) -> null $ intersect a b) (allpairs ll)) then
    return ""
  else do
    let list = zip ll $ map toInteger [1..nbHandlers]
    lockst <- tryAnyDeep $ attributeLocksMonitor list nbLocks cputimelim
    if (isLeft lockst) then return ""
    else do
      let (Right locks) = lockst
      let optMon = (AST.Monitor (Unique (show nbHandlers ++ ", " ++ (show nbRessources) ++ ", " ++ (show nbLocks)) 1) (map (\(s, n) -> dummyHandler s n) list) AST.MonitorDefined mempty [(LockCoarsening $ OptMonitor locks)])
      (retMon,_numberAfterOpt) <- lockOptimizeMonitor optMon
      aa <- statisticsMonitor retMon
      trace (aa) $ pure aa

runTests :: [(Int, Int, Int, Int)] -> IO ()
runTests params = do
  (testList::[String]) <- parallelInterleaved $ map runTest params
  _ <- sequence $ map putStrLn $ filter (not.null) testList
  stopGlobalPool