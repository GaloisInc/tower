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
      , attributeLocksMonitor
      , statisticsMonitor
      ) where

import Debug.Trace
import Data.List
import Data.List.Split
import System.IO
import System.Process
import System.Directory
import qualified Data.Set as Set

import qualified Ivory.Tower.AST as AST
import Ivory.Language.Syntax.Names
import Ivory.Tower.Types.Opts
import Ivory.Tower.Opts.LockCoarsening.StaticAnalysis
import Ivory.Tower.Opts.LockCoarsening.LockOptimize
import Ivory.Tower.Types.Time
import Data.Int
import Numeric


-- FOR STATS ONLY


import Data.Algorithm.MaximalCliques
import Ivory.Tower.Types.Unique


lockCoarseningName :: String
lockCoarseningName = "lockCoarsening"

statisticsMonitors :: [AST.Monitor] -> IO ()
statisticsMonitors [] = return ()
statisticsMonitors (a:b) = do
  s <- statisticsMonitor a 
  writeFile "stats.tmp" s
  statisticsMonitors b

statisticsMonitor :: AST.Monitor -> IO String
statisticsMonitor mon = do
  let monName = take 1 $ reverse $ take 1 $ reverse $ show (showUnique $ AST.monitor_name mon)
  let resList = (show $ concat $ intersperse "; " $ ressourceList)
  let nbNodesB = numberOfNodes handlerList
  let nbEdgesB = numberOfEdges isEdgeBefore handlerList
  let (densityB::Double) = 2*(fromIntegral nbEdgesB) / (fromIntegral $ nbNodesB*(nbNodesB-1))
  let nbNodesA = nbNodesB
  let nbEdgesA = numberOfEdges isEdgeAfter handlerList
  let (densityA::Double) = 2*(fromIntegral nbEdgesA) / (fromIntegral $ nbNodesA*(nbNodesA-1))
  let uncertainty = abs(densityA-densityB)/densityB
  return (monName ++ "," ++ resList ++ ", " ++ (show $ maxCliqueSize isEdgeBefore handlerList) ++ ", " ++
    (show nbNodesB) ++ ", " ++ (show nbEdgesB) ++ ", " ++ 
    (show $ length ressourceList) ++ ", " ++ (showFFloat (Just 6) densityB "") ++ ", " ++
    (show $ maxCliqueSize isEdgeAfter handlerList) ++  ", " ++ (show nbNodesA) ++  ", " ++
    (show nbEdgesA) ++ ", " ++ (show $ length lockList) ++ ", " ++ (showFFloat (Just 6) densityA "") ++ ", "++ (showFFloat (Just 6) uncertainty "") )
  where
    (Just (LockCoarsening (OptMonitor lockList))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    ressourceList = nub $ concat lockList
    handlerList = AST.monitor_handlers mon

    isEdgeBefore h1 h2 = 
      let (Just (LockCoarsening (OptHandler res1))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h1 in
      let (Just (LockCoarsening (OptHandler res2))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h2 in
      null $ intersect res1 res2

    isEdgeAfter h1 h2 = 
      let (Just (LockCoarsening (OptHandler res1))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h1 in
      let (Just (LockCoarsening (OptHandler res2))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h2 in
      let l1 = map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ lockList) $ res1 in
      let l2 = map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ lockList) $ res2 in
      null $ intersect l1 l2

    maxCliques :: (a -> a -> Bool) -> [a] -> [[a]]
    maxCliques isEdge list = getMaximalCliques isEdge list
    maxCliqueSize isEdge list = foldl (\a clique -> max a $ length clique) 0 $ maxCliques isEdge list

    numberOfEdges :: (a -> a -> Bool) -> [a] -> Int
    numberOfEdges isEdge list = 
      let hEdge = allpairs list in
      length $ filter (\(a,b) -> isEdge a b) hEdge

    numberOfNodes :: [a] -> Int
    numberOfNodes list = length list

lockCoarsening :: Int -> Int -> [Sym] -> AST.Tower -> IO AST.Tower
lockCoarsening nbLocksTotal cputimelim unsafeList ast = do
  let nbMonitors = length $ AST.tower_monitors $ cleanAST unsafeList ast
  if nbMonitors > nbLocksTotal 
    then error "insufficient locks given for lockCoarsening"
    else do
      (_,monitors) <- lockCoarseningMonitors ast (AST.tower_monitors ast) nbLocksTotal cputimelim unsafeList
      let astOpt = ast {AST.tower_transformers = ((LockCoarsening OptTower):(AST.tower_transformers ast)) , AST.tower_monitors = monitors}
      statisticsMonitors monitors
      return $ astOpt

lockCoarseningMonitors :: AST.Tower -> [AST.Monitor] -> Int -> Int -> [Sym] -> IO (Int,[AST.Monitor])
lockCoarseningMonitors _ [] _ _ _ = return (0,[])
lockCoarseningMonitors t (mon:b) nbLocksTotal cputimelim unsafeList = do
  let a = mon {AST.monitor_handlers = map (applyStaticAnalysisHandler mon) $ AST.monitor_handlers mon}
  let cleanmon = cleanMonitor unsafeList a
  if (null $ AST.monitor_handlers cleanmon) 
    then do
      (locksUsed, monitors) <- lockCoarseningMonitors t b (nbLocksTotal) cputimelim unsafeList
      return (locksUsed, (a {AST.monitor_transformers = ((LockCoarsening $ OptMonitor []):(AST.monitor_transformers a))}):monitors)
  else do
    (locksUsed, monitors) <- lockCoarseningMonitors t b (nbLocksTotal-1) cputimelim unsafeList
    let locksAvail = nbLocksTotal - locksUsed
    locks <- attributeLocksMonitor (zip (map fromSymToString $ staticAnalysisMonitor unsafeList $ cleanmon) (frequencies cleanmon)) locksAvail cputimelim
    let optMon = (a {AST.monitor_transformers = (LockCoarsening $ OptMonitor locks):(AST.monitor_transformers a)})
    (retMon,numberAfterOpt) <- lockOptimizeMonitor optMon
    return (locksUsed + numberAfterOpt, retMon:monitors)
  where
    applyStaticAnalysisHandler :: AST.Monitor -> AST.Handler -> AST.Handler
    applyStaticAnalysisHandler moni han =
      han {AST.handler_transformers = ((LockCoarsening $ OptHandler $ fromSymToString $ staticAnalysisHandler unsafeList (AST.monitor_moduledef moni) han):(AST.handler_transformers han))}
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

lockCoarseningMonitor :: AST.Tower -> AST.Monitor -> Int -> Int -> [Sym] -> IO (AST.Monitor)
lockCoarseningMonitor tow mon nbLocks cputimelim unsafeList = do
  (_, val) <- lockCoarseningMonitors tow [mon] nbLocks cputimelim unsafeList
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
    removeFile tmpName
    trace ("While lockCoarsening : " ++ show outputStatus ++ " . Try adding more cpu-time.") $ pure ()
    return $ [nub $ concat $ map fst list]

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
