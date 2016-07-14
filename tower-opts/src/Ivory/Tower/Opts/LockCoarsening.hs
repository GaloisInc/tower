{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Opts.LockCoarsening
      ( lockCoarsening
      , lockCoarseningName
      , lockCoarseningMonitors
      , lockCoarseningMonitor
      , cleanAST
      , attributeLocksMonitor
      , statisticsMonitor
      ) where

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
import Numeric

import Ivory.Tower.Opts.LockCoarsening.Maxsat

-- IMPORTS FOR COMPUTING THE STATISTICS

import Data.Algorithm.MaximalCliques
import Ivory.Tower.Types.Unique

----------

-- | The name of the lockCoarsening function. 
lockCoarseningName :: String
lockCoarseningName = "lockCoarsening"

-- | Generates statistics for a list of monitors and outputs 
-- them to a file named lockCoarsening.out
statisticsMonitors :: [AST.Monitor] -> IO ()
statisticsMonitors [] = return ()
statisticsMonitors (a:b) = do
  s <- statisticsMonitor a 
  writeFile "lockCoarsening.out" s
  statisticsMonitors b

-- | This functions takes a monitor and returns some useful statistics
-- about the lock coarsening execution on it. The statistics computed are
--   
--   * Number of handlers, locks, and resources
--   * Number of pairs of handlers that can execute in parallel in theory
--   (ie. the pair of handlers does not access to a common shared resource)
--   * Number of pairs of handlers that will execute in parral in practice
--   (ie. the pair of handlers does not take the same lock)
--   * Graph densities of the two previously defined graphs.
--   * Relative uncertainty between the two densities.
statisticsMonitor :: AST.Monitor -> IO String
statisticsMonitor mon = do
  let monName = show (showUnique $ AST.monitor_name mon)
  let monNameUnquot = drop 1 $ reverse $ drop 1 $ reverse $ monName
  let nbNodesB = numberOfNodes handlerList
  let nbEdgesB = fromIntegral $ numberOfEdges isEdgeBefore handlerList
  let (densityB::Double) = 2*nbEdgesB / (fromIntegral $ nbNodesB*(nbNodesB-1))
  let nbNodesA = nbNodesB
  let nbEdgesA = fromIntegral $ numberOfEdges isEdgeAfter handlerList
  let (densityA::Double) = 2*nbEdgesA / (fromIntegral $ nbNodesA*(nbNodesA-1))
  let uncertainty = abs(densityA-densityB)/densityB
  return (monNameUnquot ++ ", " ++                               -- monitor name
    {-resList ++ ", " ++-}                                       -- list of shared resources
    (show $ maxCliqueSize isEdgeBefore handlerList) ++ ", " ++   -- size of the biggest clique in the theoretical graph
    (show nbNodesB) ++ ", " ++                                   -- number of nodes in the theoretical graph (= nb of handlers)
    (show nbEdgesB) ++ ", " ++                                   -- number of edges in the theoretical graph (= nb of pairs of handlers that can execute in parallel)
    (show $ length ressourceList) ++ ", " ++                     -- number of shared resources
    (showFFloat (Just 6) densityB "") ++ ", " ++                 -- graph density of the theoretical graph
    (show $ maxCliqueSize isEdgeAfter handlerList) ++  ", " ++   -- size of the biggest clique in the practical graph
    (show nbNodesA) ++ ", " ++                                   -- number of nodes in the practical graph (= nb of handlers)
    (show nbEdgesA) ++ ", " ++                                   -- number of edges in the practical graph (= nb of pairs of handlers that will execute in parallel)
    (show $ length lockList) ++ ", " ++                          -- number of locks
    (showFFloat (Just 6) densityA "") ++ ", "++                  -- graph density of the practical graph
    (showFFloat (Just 6) uncertainty "") )                       -- relative uncertainty between the theoretical and the practical.
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

    maxCliqueSize :: (a -> a -> Bool) -> [a] -> Int
    maxCliqueSize isEdge list = 
      foldl (\a clique -> max a $ length clique) 0 $ maxCliques isEdge list

    numberOfEdges :: (a -> a -> Bool) -> [a] -> Int
    numberOfEdges isEdge list = 
      let hEdge = allpairs list in
      length $ filter (\(a,b) -> isEdge a b) hEdge

    numberOfNodes :: [a] -> Int
    numberOfNodes list = length list

-- | The main lockCoarsening function
lockCoarsening :: Int           -- ^ the upper bound on the number of locks that the solution should return.
               -> Int           -- ^ the cputime limit given to the PWMS solver. 
               -> [Sym]         -- ^ the list of functions (given as function symbols) that access to ressources escaping the monitor scope.
               -> AST.Tower     -- ^ the Tower ast.
               -> IO AST.Tower  -- ^ the Tower ast annotated with lock coarsening indications.
lockCoarsening nbLocksTotal cputimelim unsafeList ast = do
  let nbMonitors = length $ AST.tower_monitors $ cleanAST unsafeList ast
  if nbMonitors > nbLocksTotal 
    then error "insufficient locks given for lockCoarsening"
    else do
      (_,monitors) <- lockCoarseningMonitors ast (AST.tower_monitors ast) nbLocksTotal cputimelim unsafeList
      let astOpt = ast {AST.tower_transformers = ((LockCoarsening OptTower):(AST.tower_transformers ast)) , AST.tower_monitors = monitors}
      statisticsMonitors monitors
      return $ astOpt

-- | Does the lock coarsening to a list of monitors.
lockCoarseningMonitors :: AST.Tower              -- ^ the full tower ast.
                       -> [AST.Monitor]          -- ^ the list of monitors.
                       -> Int                    -- ^ the total number of locks available (upper bound of the number of locks).
                       -> Int                    -- ^ the cputime limit given to the PWMS solver. 
                       -> [Sym]                  -- ^ the list of functions (given as function symbols) that access to ressources escaping the monitor scope.
                       -> IO (Int,[AST.Monitor]) -- ^ Returns the number of locks used and the monitors annotated with lock coarsening indications.
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
    locks <- attributeLocksMonitor (AST.monitor_name a)
                                   (zip (staticAnalysisMonitor unsafeList $ cleanmon) (frequencies cleanmon)) 
                                   locksAvail 
                                   cputimelim
    let optMon = (a {AST.monitor_transformers = (LockCoarsening $ OptMonitor locks):(AST.monitor_transformers a)})
    (retMon,numberAfterOpt) <- lockOptimizeMonitor optMon
    return (locksUsed + numberAfterOpt, retMon:monitors)
  where
    applyStaticAnalysisHandler :: AST.Monitor -> AST.Handler -> AST.Handler
    applyStaticAnalysisHandler moni han =
      han {AST.handler_transformers = ((LockCoarsening $ OptHandler $ staticAnalysisHandler unsafeList (AST.monitor_moduledef moni) han):(AST.handler_transformers han))}
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

-- | Wrapper to apply the lock coarsening on only one monitor.
lockCoarseningMonitor :: AST.Tower -> AST.Monitor -> Int -> Int -> [Sym] -> IO (AST.Monitor)
lockCoarseningMonitor tow mon nbLocks cputimelim unsafeList = do
  (_, val) <- lockCoarseningMonitors tow [mon] nbLocks cputimelim unsafeList
  return $ head val

allpairs :: [t] -> [(t,t)]
allpairs [] = []
allpairs [_] = []
allpairs (x:xs) = concatMap (\y -> [(x,y)]) xs ++ allpairs xs

-- | internal function that calls the PWMS solver.
attributeLocksMonitor :: Unique                -- ^ the name of the monitor
                      -> [([String],Integer)]  -- ^ A list of handlers with : (list of resources, execution frequency)
                      -> Int                   -- ^ the upper bound on the number of locks for this instance.
                      -> Int                   -- ^ the cputime limit given to the PWMS solver. 
                      -> IO [[String]]         -- ^ an attribution of locks
attributeLocksMonitor mname list nbLocksPre cputimelim = do
  (tmpName, tmpHandle) <- openTempFile "." "temp"
  hPutStr tmpHandle (renderPWMS input)
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
    _ <- error ("While lockCoarsening : " ++ show outputStatus ++ " . Try adding more cpu-time.")
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

    nbLocksI :: Integer
    nbLocksI = toInteger nbLocks

    associationSet :: Set.Set String
    associationSet = Set.fromList (concat $ map (\s -> map (makeLockRaw s) [1..nbLocksI]) allStates)

    makeLockRaw :: String -> Integer -> String
    makeLockRaw state lockid = state ++ "__" ++ (show lockid)

    makeLock :: String -> Integer -> Integer
    makeLock state lockid = toInteger $ ((Set.findIndex (makeLockRaw state lockid) associationSet) +1)

    input :: PWMS
    input = PWMS
      { comments     = ["comments Partial Max-SAT", showUnique mname, (show list)]
      , hard_clauses = sanity
      , soft_clauses = softClauses
      }

    sanity :: [HardClause]
    sanity = (map oneIsTrue allStates) ++ (concat $ map couplesFalse allStates)
      where 
        oneIsTrue :: String -> HardClause
        oneIsTrue state = map (makeLock state) [1..nbLocksI]

        couplesFalse :: String -> [HardClause]
        couplesFalse state = map (\ (i,j) -> [(negate $ makeLock state i), 
          (negate $ makeLock state j)]) [(i,j) | i<-[1..(nbLocksI-1)], j<-[(i+1)..nbLocksI]]

    softClauses :: [SoftClause]
    softClauses = genClause list

    genClause :: [([String],Integer)] -> [SoftClause]
    genClause clique = 
      concat $ map createClause pairs
      where
        pairs = filter (\(x,y) -> null $ intersect (fst x) (fst y)) $ allpairs clique

    createClause :: (([String],Integer),([String],Integer)) -> [SoftClause]
    createClause (([],_),_) = []
    createClause ((x:xs, f1),(l2,f2)) = 
      if (f1*f2 > 0) 
        then (concat $ map (\str -> createLocks x str) l2) ++ (createClause ((xs,f1),(l2,f2)))
        else []
      where
        createLocks :: String -> String -> [SoftClause]
        createLocks s1 s2 = 
          map (\i -> (f1*f2, [makeLock s1 i, makeLock s2 i])) [1..nbLocksI] ++
          map (\i -> (f1*f2, [negate $ makeLock s1 i, negate $ makeLock s2 i])) [1..nbLocksI]
