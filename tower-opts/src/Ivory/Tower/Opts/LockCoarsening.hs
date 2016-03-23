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
      ) where

import Data.Algorithm.MaximalCliques
import Data.List
import Data.List.Split
import System.IO
import System.Process
import System.Directory
import qualified Data.Set as Set

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Opts
import Ivory.Tower.Opts.LockCoarsening.StaticAnalysis


lockCoarseningName :: String
lockCoarseningName = "lockCoarsening"

lockCoarsening :: Int -> Int -> AST.Tower -> IO AST.Tower
lockCoarsening nbLocksTotal cputimelim ast = do
  let nbMonitors = length $ AST.tower_monitors $ cleanAST ast
  if nbMonitors > nbLocksTotal 
    then error "insufficient locks given for lockCoarsening"
    else do
      (_,monitors) <- lockCoarseningMonitors (AST.tower_monitors ast) nbLocksTotal cputimelim
      return ast {AST.tower_transformers = ((LockCoarsening OptTower):(AST.tower_transformers ast)) , AST.tower_monitors = monitors}

  
lockCoarseningMonitors :: [AST.Monitor] -> Int -> Int -> IO (Int,[AST.Monitor])
lockCoarseningMonitors [] _ _ = return (0,[])
lockCoarseningMonitors list@(a:b) nbLocksTotal cputimelim = do
  if (null.AST.monitor_handlers $ cleanMonitor a) 
    then do
      (locksUsed, monitors) <- lockCoarseningMonitors b (nbLocksTotal) cputimelim
      return (locksUsed, (a {AST.monitor_transformers = ((LockCoarsening $ OptMonitor []):(AST.monitor_transformers a))}):monitors)
  else do
    (locksUsed, monitors) <- lockCoarseningMonitors b (nbLocksTotal-1) cputimelim
    let locksAvail = nbLocksTotal - locksUsed
    locks <- attributeLocksMonitor (map fromSymToString $ staticAnalysisMonitor $ cleanMonitor a) locksAvail cputimelim
    return (locksUsed + (length locks), (a {AST.monitor_transformers = (LockCoarsening $ OptMonitor locks):(AST.monitor_transformers a)}):monitors)

lockCoarseningMonitor :: AST.Monitor -> Int -> Int -> IO (AST.Monitor)
lockCoarseningMonitor mon nbLocks cputimelim = do
  (_, val) <- lockCoarseningMonitors [mon] nbLocks cputimelim
  return $ head val



cleanAST :: AST.Tower -> AST.Tower
cleanAST ast = ast {AST.tower_monitors = filter (not.null.AST.monitor_handlers) $ map cleanMonitor $ AST.tower_monitors ast}

cleanMonitor :: AST.Monitor -> AST.Monitor
cleanMonitor mon = 
    mon {AST.monitor_handlers = filter (not.null.staticAnalysisHandler) (AST.monitor_handlers mon)}


isEdge :: [String] -> [String] -> Bool
isEdge a b= null $ intersect a b

allpairs :: [t] -> [(t,t)]
allpairs [] = []
allpairs [_] = []
allpairs (x:xs) = concatMap (\y -> [(x,y)]) xs ++ allpairs xs


attributeLocksMonitor :: [[String]] -> Int -> Int -> IO [[String]]
attributeLocksMonitor list nbLocksPre cputimelim = do
  (tmpName, tmpHandle) <- openTempFile "." "temp"
  hPutStr tmpHandle (concat $ intersperse "\n" input)
  hFlush tmpHandle
  hClose tmpHandle
  (_, out, _) <- readProcessWithExitCode "open-wbo" ["-cpu-lim="++(show cputimelim), tmpName] ""
  let outputLine = drop 2 $ (concat $ filter (\x -> compare "v" (take 1 x) == EQ) (lines out))
  let (list2::[Int]) = filter (\x -> x>=0) $ map read (words outputLine)
  let sol = map (\x -> Set.elemAt (x-1) associationSet) list2
  let sortsol = filter (not.null) $ map (\i -> concat $ map (\s -> keepString s i) sol) [1..nbLocks]
  removeFile tmpName
  return sortsol

  where
    keepString::String->Int->[String]
    keepString s i =
      let ni = reverse $ head $ splitOn "__" (reverse s) in
      if ((read ni) == i) then ([take ((length s) - (2 + (length ni))) s]) else []

    allStates :: [String]
    allStates = nub $ concat $ list

    nbLocks :: Int
    nbLocks = minimum [nbLocksPre, (length allStates)]

    associationSet :: Set.Set String
    associationSet = Set.fromList (concat $ map (\s -> map (makeLockRaw s) [1..nbLocks]) allStates)

    maxCliques :: [[[String]]]
    maxCliques = getMaximalCliques isEdge list


    makeLockRaw :: String -> Int -> String
    makeLockRaw state lockid = state ++ "__" ++ (show lockid)

    makeLock :: String -> Int -> String
    makeLock state lockid = show ((Set.findIndex (makeLockRaw state lockid) associationSet) +1)

    input :: [String]
    input = ["c comments Partial Max-SAT", "p wcnf "++(show nbVar)++" "++(show.length $ logicformula)++" "++(show hardWeight)] ++ (map replaceWeights logicformula)
      where
        hardWeight :: Int
        hardWeight = (length softClauses) + 10

        replaceWeights :: String -> String
        replaceWeights str =
          if (compare "hard" (take 4 str) == EQ ) 
            then
              (show hardWeight)++(drop 4 str)
            else
              (show (1::Integer)) ++ (drop 4 str)

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
    softClauses = concat $ map genClause maxCliques

    genClause :: [[String]] -> [String]
    genClause clique = 
      concat $ map createClause pairs
      where
        pairs = allpairs clique

    createClause :: ([String],[String]) -> [String]
    createClause ([],_) = []
    createClause ((x:xs),l2) = 
      (concat $ map (\str -> createLocks x str) l2) ++ (createClause (xs,l2))
      where
        createLocks :: String -> String -> [String]
        createLocks s1 s2 = 
          map (\i -> "soft "++(makeLock s1 i) ++ " " ++(makeLock s2 i)) [1..nbLocks] ++
          map (\i -> "soft -"++(makeLock s1 i) ++ " -" ++(makeLock s2 i)) [1..nbLocks]
