
module Ivory.Tower.Compile.AADL.SystemDoc
  ( systemDoc
  ) where

import Control.Applicative ((<$>))
import Data.List (sortBy, groupBy)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.FilePath

import           Ivory.Compile.AADL.AST
-- import           Ivory.Compile.AADL.Identifier
import           Ivory.Compile.AADL.Monad
import           Ivory.Compile.AADL.Gen (mkType)

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Tower.Types.Unique
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.Directory as D

type CompileAADL = CompileM (Unique,[String])

systemDoc :: String -> [Module] -> AST.System p
          -> TypeCtxM (Document, [Warning])
systemDoc name ms sysast = runCompile ms virtMod $ do
  writeImport "SMACCM_SYS"
  tg <- threadGroup [] (AST.system_tasks sysast)
  pd <- processDef name sysast
  writeThreadGroup tg
  writeProcessDefinition pd
  where
  virtMod = package name $ do
    mapM_ depend ms


type ThreadGroup = [ThreadDef] -- XXX change later.

writeThreadGroup :: ThreadGroup -> CompileAADL ()
writeThreadGroup = mapM_ writeThreadDefinition

threadGroup :: [Unique] -> D.Dir Unique (AST.Task p) -> CompileAADL ThreadGroup
threadGroup path (D.Dir ts subdirs) = do
  tdefs   <- mapM threadDef ts
  subdefs <- mapM recur   subdirs
  return (concat (tdefs : subdefs))
  where
  recur (D.Subdir n d) = threadGroup (n:path) d


threadDef :: AST.Task p -> CompileAADL ThreadDef
threadDef t = do
  threadname <- introduceUnique (unique_name (AST.task_name t))
                                (AST.task_name t) []
  features <- featuresDef threadname t (loopsource <.> "h")
  let props = [ ThreadProperty "Source_Text"
                  (PropList [PropString (usersource <.> "c")])
              , ThreadProperty "Initialize_Source_Text"
                  (PropString ("task_init_" ++ (showUnique (AST.task_name t))))
              , ThreadProperty "Priority"
                  (PropInteger (AST.task_priority t))
              , ThreadProperty "Source_Stack_Size"
                  (PropUnit (AST.task_stack_size t) "bytes")
              , ThreadProperty "SMACCM_SYS::Language"
                  (PropString "Ivory")
              ] ++ periodprops

  return (ThreadDef threadname features props)
  where
  loopsource = "tower_task_loop_" ++ (showUnique (AST.task_name t))
  usersource = "tower_task_loop_" ++ (showUnique (AST.task_name t))

  periodprops = case uniqueperiods of
    [(p, hs)] -> mkPeriodProperty p hs
    [] -> [ThreadProperty "Dispatch_Protocol" (PropLiteral "Sporadic")]
    _  -> trace warnmsg $ [UnprintableThreadProperty warnmsg]
    where
    warnmsg = "Warning: multiple periodic rates in a tower task named "
              ++ (showUnique (AST.task_name t))
              ++ " cannot be rendered into an AADL property."
    uniqueperiods :: [(Integer, [AST.Handler])]
    uniqueperiods = map (\xs -> (fst (head xs), map snd xs))
      $ groupBy (\a b -> fst a == fst b)
      $ sortBy  (\a b -> fst a `compare` fst b)
      $ catMaybes
      $ map aux (AST.task_handlers t)
    aux eh@(AST.Handler _ _ (AST.TimerEvt timer)) =
                                          Just (AST.timer_per timer, eh)
    aux _ = Nothing
  mkPeriodProperty interval handlers =
    [ ThreadProperty "Dispatch_Protocol" (PropLiteral "Hybrid")
    , ThreadProperty "Period" (PropUnit interval "us")
    , ThreadProperty "SMACCM_SYS::Compute_Entrypoint_Source_Text"
        (PropList [ PropString (showUnique (AST.handler_name h))
                  | h <- handlers])
    ]


featuresDef :: String -> AST.Task p -> FilePath -> CompileAADL [ThreadFeature]
featuresDef scope taskast headername = do
  es  <- mapM emitterDef          (AST.task_chan_emitters        taskast)
  prs <- mapM pollReceiverDef     (AST.task_chan_poll_receivers  taskast)
  ers <- mapM eventReceiverDef    (AST.task_chan_event_receivers taskast)
  rs  <- mapM readerDef           (AST.task_chan_readers         taskast)
  return $ concat [es, prs, ers, rs]
  where
  emitterDef :: AST.ChanEmitter -> CompileAADL ThreadFeature
  emitterDef ce = do
    chtype <- channelTypename (AST.chanemitter_chan ce)
    portname <- introduceUnique (AST.chanemitter_annotation ce)
                                (AST.chanemitter_name ce) [scope]
    let ps = channelprops (AST.chanemitter_name ce)
                          (AST.chan_size (AST.chanemitter_chan ce))
    return $ ThreadFeatureEventPort portname Out chtype ps

  pollReceiverDef ::AST.ChanReceiver -> CompileAADL ThreadFeature
  pollReceiverDef cr = do
    chtype <- channelTypename (AST.chanreceiver_chan cr)
    portname <- introduceUnique (AST.chanreceiver_annotation cr)
                                (AST.chanreceiver_name cr) [scope]
    let ps = channelprops (AST.chanreceiver_name cr)
                          (AST.chan_size (AST.chanreceiver_chan cr))
        p = ThreadProperty "ReceiverType" (PropString "Poll") -- XXX hack
    return $ ThreadFeatureEventPort portname In chtype (p:ps)

  eventReceiverDef ::AST.ChanReceiver -> CompileAADL ThreadFeature
  eventReceiverDef cr = do
    chtype <- channelTypename (AST.chanreceiver_chan cr)
    portname <- introduceUnique (AST.chanreceiver_annotation cr)
                                (AST.chanreceiver_name cr) [scope]
    let cps = channelprops (AST.chanreceiver_name cr)
                           (AST.chan_size (AST.chanreceiver_chan cr))
        rtyp = [ThreadProperty "ReceiverType" (PropString "Event")] -- XXX hack

        handlers = map (PropString . showUnique . AST.handler_name)
                 $ filter (\eh -> AST.handler_evt eh
                              == AST.ChanEvt (AST.chanreceiver_chan cr) cr)
                          (AST.task_handlers taskast)
        hps = [ThreadProperty "SMACCM_SYS::Compute_Entrypoint_Source_Text"
                              (PropList handlers)]
        ps = concat [rtyp, cps, hps]
    return $ ThreadFeatureEventPort portname In chtype ps

  readerDef :: AST.ChanReader -> CompileAADL ThreadFeature
  readerDef cr = do
    chtype <- channelTypename (AST.chanreader_chan cr)
    portname <- introduceUnique (AST.chanreader_annotation cr)
                                (AST.chanreader_name cr) [scope]
    return $ ThreadFeatureDataPort portname chtype []

  channelprops :: Unique -> Integer -> [ThreadProperty]
  channelprops usourcetext chansize =
    [ ThreadProperty "Queue_Size" (PropInteger chansize)
    , smaccmProp "CommPrim_Source_Header" headername
    , smaccmProp "CommPrim_Source_Text"   (showUnique usourcetext) -- XXX fix once we do codegen
    ]

channelTypename :: AST.Chan -> CompileAADL TypeName
channelTypename chan = do
  t <- mkType (AST.chan_ityp chan)
  return $ implNonBaseTypes t
  where
  -- HACK: user declared datatypes always need .impl appended
  implNonBaseTypes :: TypeName -> TypeName
  implNonBaseTypes t = case t of
      QualTypeName "Base_Types" _ -> t
      DotTypeName _ _ -> t
      _ -> DotTypeName t "impl"

processDef :: String -> AST.System p -> CompileAADL ProcessDef
processDef name sys = do
  pcs <- processComponents (AST.system_tasks sys)
  conns <- chanConns (map snd (D.flatten (AST.system_tasks sys)))
  -- Give each connection a meaningless but unique name
  let namedconns = zipWith (\c i -> NamedConnection ("conn_" ++ show i) c)
                           conns [1::Int ..]
  return (ProcessDef name pcs namedconns)
  where
  processComponents :: D.Dir Unique (AST.Task p)
                    -> CompileAADL [ProcessComponent]
  processComponents (D.Dir ts subdirs) = do
    as  <- mapM component ts
    aas <- mapM recur subdirs
    return (concat (as:aas))
  component :: AST.Task p -> CompileAADL ProcessComponent
  component t = do
    name' <- uniqueIdentifier (AST.task_name t) "processComponent"
    return (ProcessThread (threadInstance name') name')
  recur (D.Subdir _ d) = processComponents d -- Not treating grouping

  chanConns :: [AST.Task p] -> CompileAADL [ProcessConnection]
  chanConns ts = do
    is <- concat <$> mapM inboundConns ts
    os <- concat <$> mapM outboundConns ts
    return [ EventConnection opc ipc
           | (ichan, ipc) <- is
           , (ochan, opc) <- os
           , ichan == ochan
           ]
    where
    inboundConns t = do
      prs <- mapM (receiverEndpoint t) (AST.task_chan_poll_receivers t)
      ers <- mapM (receiverEndpoint t) (AST.task_chan_event_receivers t)
      rrs <- mapM (readerEndpoint t)   (AST.task_chan_readers t)
      return (concat [prs, ers, rrs])
    outboundConns t = mapM (emitterEndpoint t) (AST.task_chan_emitters t)

  receiverEndpoint :: AST.Task p -> AST.ChanReceiver -> CompileAADL (AST.Chan ,ProcessPort)
  receiverEndpoint t r = do
    tt <- uniqueIdentifier (AST.task_name t) "receiverEndpoint task name"
    rr <- uniqueIdentifier (AST.chanreceiver_name r) "receiverEndpoint receiver name"
    return (AST.chanreceiver_chan r, (ProcessPort (threadInstance tt) rr))

  readerEndpoint :: AST.Task p -> AST.ChanReader -> CompileAADL (AST.Chan, ProcessPort)
  readerEndpoint t r = do
    tt <- uniqueIdentifier (AST.task_name t) "readerEndpoint task name"
    rr <- uniqueIdentifier (AST.chanreader_name r) "readerEndpoint reader name"
    return (AST.chanreader_chan r, (ProcessPort (threadInstance tt) rr))

  emitterEndpoint :: AST.Task p -> AST.ChanEmitter -> CompileAADL (AST.Chan, ProcessPort)
  emitterEndpoint t e = do
    tt <- uniqueIdentifier (AST.task_name t) "emitterEndpoint task name"
    ee <- uniqueIdentifier (AST.chanemitter_name e) "emitterEndpoint emitter name"
    return (AST.chanemitter_chan e, (ProcessPort (threadInstance tt) ee))

threadInstance :: String -> String
threadInstance t = t ++ "_inst"

-- Uniqueness managment -------------------------------------------------------

introduceUnique :: String -> Unique -> [String] -> CompileAADL String
introduceUnique s u scope = do
  m <- getIdentifierMap
  let mm = filter ((== scope) . snd . fst) m
  case elem s (map snd mm)  of
    False -> do
      setIdentifierMap (((u,scope),s):m)
      return s
    True -> do
      uniquenessWarning warning
      setIdentifierMap (((u,scope),us):m)
      return us
  where
  us = showUnique u
  warning = "User provided identifier \"" ++ s ++ "\" for " ++ us
         ++ " is not unique in tower-aadl Assembly."

uniqueIdentifier :: Unique -> String -> CompileAADL String
uniqueIdentifier u ctx = do
  m <- getIdentifierMap
  -- If this lookup fails, its because the CompileM code that should have used
  -- introduceUnique to create names was not implemented correctly.
  case lookup u (map withoutScope m) of
    Just n -> return n
    Nothing -> error ("Failed to find unique identifier " ++ (showUnique u)
                      ++ " in " ++ ctx ++ " context.\nName Map Dump\n" ++ (show m))
  where
  withoutScope ((k,_s),v) = (k,v)

-- Constructor Helper Functions -----------------------------------------------

threadProp :: String -> String -> ThreadProperty
threadProp k v = ThreadProperty k (PropString v)

smaccmProp :: String -> String -> ThreadProperty
smaccmProp k v = threadProp ("SMACCM_SYS::" ++ k) v



