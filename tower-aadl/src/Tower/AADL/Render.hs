--
-- Render AADL source text from the AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render
 ( renderSystem
 ) where

import Prelude hiding (id)

import Tower.AADL.AST
import Tower.AADL.AST.Common
import Tower.AADL.Compile
import Tower.AADL.Priorities(Priority(..))
import Tower.AADL.Render.Common
import Tower.AADL.Render.Types

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------

-- | Render a system
renderSystem :: System -> CompiledDocs
renderSystem s = compiledDocs sys thds Nothing
  where
  sys  = compiledDoc SystemDoc (systemName s) sysRen
  thds = map go (concatMap processComponents $ systemComponents s)
    where
    go thd = compiledDoc ThreadDoc (threadName thd) (renderThread thd)

  sysRen = vsep (map renderProcess (systemComponents s))
     <$$$> renderBlk (text "system") nm []
     <$$$> renderBlk (text "system" <+> text "implementation")
                     (mkImpl nm) blk

  nm  = text (systemName s)
  blk =
    [ text "subcomponents"
    , tab $ vsep $ map renderSystemSubcomponent $ zip [0..] $ systemComponents s
    , text "properties"
    , tab $ vsep $ map renderSystemProperty $ systemProperties s
    ]

renderSystemSubcomponent :: (Integer, Process) -> Doc
renderSystemSubcomponent (i,p) = stmt $
      text ("p" ++ show i)
  <>  colon
  <>  text "process"
  <+> mkImpl (text (processName p))

renderSystemProperty :: SystemProperty -> Doc
renderSystemProperty p = case p of
  SystemOS os
    -> stmt (fromSMACCM (text "OS") ==> text os)
  SystemHW hw
    -> stmt (fromSMACCM (text "HW") ==> text hw)

renderProcess :: Process -> Doc
renderProcess p =
        renderBlk (text "process") nm []
  <$$$> renderBlk (text "process" <+> text "implementation") (mkImpl nm) blk
  where
  blk =
    [ text "subcomponents"
    , tab $ vsep $ map renderProcessSubcomponent namedThreads
    ] ++ connections

  connections =
    let chans = threadsChannels namedThreads in
    if emptyConnections (filterEndpoints chans) then []
      else [ text "connections"
           , tab $ renderConnections (allConnections chans)
           ]
  namedThreads = zip threads (map (("th"++) . show) [0::Integer ..])
  threads      = processComponents p
  nm           = text (processName p)

renderProcessSubcomponent :: (Thread, LocalId) -> Doc
renderProcessSubcomponent (t, var) = stmt $
  text var <> colon <+> text "thread" <+> nameSpace nm nm
  where
  nm = text (threadName t)

-- Channel connections in a process
renderConnections :: [ThdIds]
                  -> Doc
renderConnections thdLst =
  let cs = concatMap renderConnection thdLst in
  vsep $ zipWith syncConnection [(0::Int) ..] cs
  where
  renderConnection thds = do
    tx <- getTxThds thds
    rx <- getRxThds thds
    return (tx, rx)

  syncConnection i (txThd, rxThd) = stmt
      $ syncConnectId i ltx lrx
     <> colon
    <+> text "port"
    <+> (text ltx <> dot <> mkTxChan (snd txThd))
    ->> (text lrx <> dot <> mkRxChan (snd rxThd))
    where
    ltx = fst txThd
    lrx = fst rxThd

  syncConnectId i txThd rxThd =
    text (txThd ++ "_to_" ++ rxThd ++ "_" ++ show i)

renderThread :: Thread -> Doc
renderThread t =
  renderBlk (text "thread") (text (threadName t)) $
    map renderComment (threadComments t)
    ++
    [ text "features"
    , tab $ vsep $ map renderThreadFeature $ threadFeatures t
    , text "properties"
    , tab $ vsep $ map renderThreadProperty $ threadProperties t
    ]

renderThreadFeature :: Feature -> Doc
renderThreadFeature f = case f of
  InputFeature  rx
    -> renderInput rx
  OutputFeature tx
    -> renderOutput tx
  SignalFeature s
    -> renderSignal s

renderInput :: Input -> Doc
renderInput rx = stmt
    $ mkRxChan (inputLabel rx) <> colon
  <+> text "in"
  <+> edp
  <+> renderTypeNS Other (inputType rx)
 <$$> chanSrc (vsep $ entry : src ++ snds ++ queue ++ sndsEvents)
  where
  (fps, syms) = unzip $ inputCallback rx
  entry = renderEntryPoint syms
  src = if emptyStrs fps then []
          else [renderSrcText fps]
  snds = if emptyStrs fps
           -- Send events nowhere for external threads
           then [renderSendsEventsTo []]
           else []
  sndsEvents = [renderSendsEventsTo (inputSendsEvents rx)]
  queue = maybe [] q (inputQueue rx)
    where q sz = [ stmt $ text "Queue_Size" ==> integer sz ]
  emptyStrs = all null

renderOutput :: Output -> Doc
renderOutput tx = stmt
    $ mkTxChan (outputLabel tx) <> colon
  <+> text "out"
  <+> edp
  <+> renderTypeNS Other (outputType tx)
 <$$> chanSrc st
  where
  st = stmt $ fromSMACCM primSrc ==> dquotes (text (outputEmitter tx))

renderSignal :: SignalInfo -> Doc
renderSignal s = stmt
    $ mkRxChan (signalInfoName s) <> colon
  <+> text "in"
  <+> hsep (map text ["event", "port"])
 <$$> chanSrc (vsep (entry : isrStmts ++ src ++ sndsEvents))
  where
  (fps, syms) = unzip $ signalInfoCallback s
  entry       = renderEntryPoint syms
  src         = if emptyStrs fps
                  then []
                  else [renderSrcText fps]
  sndsEvents  = [renderSendsEventsTo (signalInfoSendsEvents s)]
  emptyStrs   = all null
  isrStmts    = [isISR
                ,firstLevelHandler (signalInfoName s)
                ,sigName "external_irq"
                ,sigNum  (signalInfoNumber s)
                ]

edp :: Doc
edp = hsep (map text ["event", "data", "port"])

chanSrc :: Doc -> Doc
chanSrc d =
      tab lbrace
 <$$> tab (tab d)
 <$$> tab rbrace

renderSendsEventsTo :: SendsEvents -> Doc
renderSendsEventsTo sevs =
       stmt
     $ fromSMACCM sendsEventsTo
   ==> dquotes (braces (braces (mkOuts sevs)))
  where
  mkOuts txs = hsep (punctuate comma (map go txs))
    where go (chanLabel,bnd) = integer bnd <+> mkTxChan chanLabel

renderThreadProperty :: ThreadProperty -> Doc
renderThreadProperty p = case p of
  DispatchProtocol per
    -> mkDisptach per
  ThreadType ty
    -> mkThreadType ty
  External
    -> external
  ExecTime (l, h)
    -> stmt
     $ text "Compute_Execution_Time"
   ==> prettyTime l <+> dot <> dot <+> prettyTime h
  StackSize sz
    -> stmt (text "Stack_Size" ==> integer sz <+> text "bytes")
  Priority (P pri)
    -> stmt (text "Priority" ==> int pri)
  EntryPoint syms
    -> renderEntryPoint syms
  SourceText srcs
    -> renderSrcText srcs
  SendEvents sevs
    -> renderSendsEventsTo sevs
  InitProperty sym
    -> stmt $ initEntryPoint ==> dquotes (text sym)
  where
  mkDisptach per = case per of
    Periodic i
      ->   mkDispatchdec (text "Periodic")
      <$$> stmt (text "Period" ==> prettyTime i)
    Aperiodic
      -> mkDispatchdec (text "Aperiodic")
    Sporadic
      -> mkDispatchdec (text "Sporadic")
    Signal{}
      -> error "Signals not implemented in AADL backend."
  mkDispatchdec dis = stmt (text "Dispatch_Protocol" ==> dis)
  mkThreadType ty = stmt (fromSMACCM (text "Thread_Type" ==> renderTy))
    where renderTy = case ty of
                       Passive -> text "Passive"
                       Active  -> text "Active"

renderEntryPoint :: [FuncSym] -> Doc
renderEntryPoint syms = stmt $ fromSMACCM entrySrc ==> renderLs syms

renderSrcText :: [FilePath] -> Doc
renderSrcText srcs = stmt $ srcText ==> renderLs srcs


