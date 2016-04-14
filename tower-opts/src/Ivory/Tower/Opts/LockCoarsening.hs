{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Opts.LockCoarsening
      ( lockCoarsening
      , lockCoarseningName
      , lockCoarseningMonitors
      , lockCoarseningMonitor
      , cleanAST
      ) where

import Data.List
import Data.List.Split
import System.IO
import System.Process
import System.Directory
import qualified Data.Set as Set

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Opts
import Ivory.Tower.Opts.LockCoarsening.StaticAnalysis
import Ivory.Tower.Opts.LockCoarsening.LockOptimize
import Ivory.Tower.Types.Time
import Data.Int

lockCoarseningName :: String
lockCoarseningName = "lockCoarsening"

lockCoarsening :: Int -> Int -> AST.Tower -> IO AST.Tower
lockCoarsening nbLocksTotal cputimelim ast = do
  let nbMonitors = length $ AST.tower_monitors $ cleanAST ast
  if nbMonitors > nbLocksTotal 
    then error "insufficient locks given for lockCoarsening"
    else do
      (_,monitors) <- lockCoarseningMonitors ast (AST.tower_monitors ast) nbLocksTotal cputimelim
      let astOpt = ast {AST.tower_transformers = ((LockCoarsening OptTower):(AST.tower_transformers ast)) , AST.tower_monitors = monitors}
      return $ astOpt

lockCoarseningMonitors :: AST.Tower -> [AST.Monitor] -> Int -> Int -> IO (Int,[AST.Monitor])
lockCoarseningMonitors _ [] _ _ = return (0,[])
lockCoarseningMonitors t (mon:b) nbLocksTotal cputimelim = do
  let a = mon {AST.monitor_handlers = map (applyStaticAnalysisHandler mon) $ AST.monitor_handlers mon}
  let cleanmon = cleanMonitor a
  if (null $ AST.monitor_handlers cleanmon) 
    then do
      (locksUsed, monitors) <- lockCoarseningMonitors t b (nbLocksTotal) cputimelim
      return (locksUsed, (a {AST.monitor_transformers = ((LockCoarsening $ OptMonitor []):(AST.monitor_transformers a))}):monitors)
  else do
    (locksUsed, monitors) <- lockCoarseningMonitors t b (nbLocksTotal-1) cputimelim
    let locksAvail = nbLocksTotal - locksUsed
    locks <- attributeLocksMonitor (zip (map fromSymToString $ staticAnalysisMonitor $ cleanmon) (frequencies cleanmon)) locksAvail cputimelim
    let optMon = (a {AST.monitor_transformers = (LockCoarsening $ OptMonitor locks):(AST.monitor_transformers a)})
    (retMon,numberAfterOpt) <- lockOptimizeMonitor optMon
    --TODO correct length locks
    return (locksUsed + numberAfterOpt, retMon:monitors)
  where
    applyStaticAnalysisHandler :: AST.Monitor -> AST.Handler -> AST.Handler
    applyStaticAnalysisHandler moni han =
      han {AST.handler_transformers = ((LockCoarsening $ OptHandler $ fromSymToString $ staticAnalysisHandler (AST.monitor_moduledef moni) han):(AST.handler_transformers han))}
    frequencies :: AST.Monitor -> [Integer]
    frequencies moni = 
      let han = AST.monitor_handlers moni in
      let threads = AST.towerThreads t in
      let thr = map (\h -> filter (\e -> (AST.handler_name h) `elem` (map (AST.handler_name . AST.handler.snd) (AST.threadHandlers (AST.messageGraph t) e))) threads) han in
      map frequency (map (\x -> filter allNonInit x) $ thr)
      where 
        allNonInit (AST.InitThread _)= False 
        allNonInit _ = True
        extractSignal (AST.SignalThread th)= [th]
        extractSignal _ = []
        extractPeriod (AST.PeriodThread pe)= [pe]
        extractPeriod _ = []

        frequency :: [AST.Thread] -> Integer
        frequency [] = 0
        frequency ll = 
          let sig = map (AST.signal_deadline) $ concat $ map extractSignal ll in 
          let per = map (AST.period_dt) $ concat $ map extractPeriod ll in 
          if null sig 
            then freqOf $ minimum per
            else freqOf $ minimum (sig++per)
          where
            freqOf (Microseconds f) = quot 100000000 f

lockCoarseningMonitor :: AST.Tower -> AST.Monitor -> Int -> Int -> IO (AST.Monitor)
lockCoarseningMonitor tow mon nbLocks cputimelim = do
  (_, val) <- lockCoarseningMonitors tow [mon] nbLocks cputimelim
  return $ head val

allpairs :: [t] -> [(t,t)]
allpairs [] = []
allpairs [_] = []
allpairs (x:xs) = concatMap (\y -> [(x,y)]) xs ++ allpairs xs


attributeLocksMonitor :: [([String],Integer)] -> Int -> Int -> IO [[String]]
attributeLocksMonitor list nbLocksPre cputimelim = do
  (tmpName, tmpHandle) <- openTempFile "." "temp"
  hPutStr tmpHandle (concat $ intersperse "\n" input)
  hFlush tmpHandle
  hClose tmpHandle
  (_, out, _) <- readProcessWithExitCode "open-wbo" ["-cpu-lim="++(show cputimelim), tmpName] ""
  let outputStatus = drop 2 $ (concat $ filter (\x -> compare "s" (take 1 x) == EQ) (lines out))
  if compare "OPTIMUM FOUND" (take 13 outputStatus) == EQ || compare "SATISFIABLE" (take 11 outputStatus) == EQ
    then do
    let outputLine = drop 2 $ (concat $ filter (\x -> compare "v" (take 1 x) == EQ) (lines out))
    let (list2::[Int]) = filter (\x -> x>=0) $ map read (words outputLine)
    let sol = map (\x -> Set.elemAt (x-1) associationSet) list2
    let sortsol = filter (not.null) $ map (\i -> concat $ map (\s -> keepString s i) sol) [1..nbLocks]
    removeFile tmpName
    return sortsol
  else do
    --removeFile tmpName
    error $ "While lockCoarsening : " ++ show outputStatus ++ " . Try adding more cpu-time."

  where
    keepString::String->Int->[String]
    keepString s i =
      let ni = reverse $ head $ splitOn "__" (reverse s) in
      if ((read ni) == i) then ([take ((length s) - (2 + (length ni))) s]) else []

    allStates :: [String]
    allStates = nub $ concat $ map fst list

    nbLocks :: Int
    nbLocks = minimum [nbLocksPre, (length allStates)]

    associationSet :: Set.Set String
    associationSet = Set.fromList (concat $ map (\s -> map (makeLockRaw s) [1..nbLocks]) allStates)

    makeLockRaw :: String -> Int -> String
    makeLockRaw state lockid = state ++ "__" ++ (show lockid)

    makeLock :: String -> Int -> String
    makeLock state lockid = show ((Set.findIndex (makeLockRaw state lockid) associationSet) +1)

    input :: [String]
    input = ["c comments Partial Max-SAT", "c "++(show list), "p wcnf "++(show nbVar)++" "++(show.length $ logicformula)++" "++(show hardWeight)] ++ (filter (not.null) $ map replaceWeights logicformula)
      where
        hardWeight :: Integer
        hardWeight = toInteger $ (maxBound::Int32) - 10

        maxValue :: Integer
        maxValue = 
          let soft = filter (\x -> (compare "hard" (take 4 x) /= EQ ) ) logicformula in
          let (values::[Integer]) = map (read . head . words . (drop (4::Int))) soft in  maximum values

        replaceWeights :: String -> String
        replaceWeights str =
          if (compare "hard" (take 4 str) == EQ ) 
            then
              (show hardWeight)++(drop (4::Int) str)
            else
              let value = read $ head $ words $ drop 4 str in
              let remaining = unwords $ tail $ words $ drop 4 str in
              if (maxValue <= 60000) 
                then (show value)++" "++remaining
                else if (quot (value*60000+maxValue-1) maxValue == 0) 
                  then ""
                  else (show (quot (value*60000+maxValue-1) maxValue))++" "++remaining

    nbVar :: Int
    nbVar = (length allStates)*nbLocks

    logicformula :: [String]
    logicformula = map (\x -> x ++ " 0") (sanity ++ softClauses)

    sanity :: [String]
    sanity = (map oneIsTrue allStates) ++ (concat $ map couplesFalse allStates)
      where 
        oneIsTrue :: String -> String
        oneIsTrue state = "hard " ++ (concat $ intersperse " " $ map (makeLock state) [1..nbLocks])

        couplesFalse :: String -> [String]
        couplesFalse state = map (\ (i,j) -> "hard -"++(makeLock state i) ++ 
          " -" ++ (makeLock state j)) [(i,j) | i<-[1..(nbLocks-1)], j<-[(i+1)..nbLocks]]

    softClauses :: [String]
    softClauses = genClause list

    genClause :: [([String],Integer)] -> [String]
    genClause clique = 
      concat $ map createClause pairs
      where
        pairs = filter (\(x,y) -> null $ intersect (fst x) (fst y)) $ allpairs clique

    createClause :: (([String],Integer),([String],Integer)) -> [String]
    createClause (([],_),_) = []
    createClause ((x:xs, f1),(l2,f2)) = 
      if (f1*f2 > 0) 
        then (concat $ map (\str -> createLocks x str) l2) ++ (createClause ((xs,f1),(l2,f2)))
        else []
      where
        createLocks :: String -> String -> [String]
        createLocks s1 s2 = 
          map (\i -> "soft"++ (show $ f1*f2) ++" "++(makeLock s1 i) ++ " " ++(makeLock s2 i)) [1..nbLocks] ++
          map (\i -> "soft"++ (show $ f1*f2) ++" -"++(makeLock s1 i) ++ " -" ++(makeLock s2 i)) [1..nbLocks]
