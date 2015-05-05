--
-- Render AADL source text from the AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render
 ( renderSystem
 ) where

import Tower.AADL.AST
import Tower.AADL.AST.Common
import Tower.AADL.Compile
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
           , tab $ vsep $ mapConnections renderConnection chans
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
renderConnection :: ChanLabel -> ChanIds -> Doc
renderConnection l labels = vsep allSynchConnect
  where
  synchConnectLabel tx rx = text (tx ++ "_to_" ++ rx)

  allSynchConnect = do
    tx <- getTxLabels labels
    rx <- getRxLabels labels
    return (synchConnection tx rx)

  synchConnection tx rx = stmt
      $ synchConnectLabel tx rx
     <> colon
    <+> text "port"
    <+> (text tx <> dot <> mkTxChan l)
    ->> (text rx <> dot <> mkRxChan l)

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

renderInput :: Input -> Doc
renderInput rx = stmt
    $ mkRxChan (inputLabel rx) <> colon
  <+> text "in"
  <+> edp
  <+> renderTypeNS (inputType rx)
 <$$> chanSrc (vsep $ entry : src ++ snds)
  where
  (fp, sym) = inputCallback rx
  entry = renderEntryPoint [sym]
  src = if null fp then []
          else [renderSrcText [fp]]
  snds = if null fp
           -- Send events nowhere for external threads
           then [renderSendsEventsTo []]
           else []

renderOutput :: Output -> Doc
renderOutput tx = stmt
    $ mkTxChan (outputLabel tx) <> colon
  <+> text "out"
  <+> edp
  <+> renderTypeNS (outputType tx)
 <$$> chanSrc st
  where
  st = stmt $ fromSMACCM primSrc ==> dquotes (text (outputEmitter tx))

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
    where go (tx,bnd) = integer bnd <+> mkTxChan (outputLabel tx)

renderThreadProperty :: ThreadProperty -> Doc
renderThreadProperty p = case p of
  DispatchProtocol per
    -> mkDisptach per
  ThreadType ty
    -> mkThreadType ty
  External
    -> stmt $ fromSMACCM (text "Is_External") ==> text "true"
  ExecTime l h
    -> stmt
     $ text "Compute_Execution_Time"
   ==> prettyTime l <+> dot <> dot <+> prettyTime h
  StackSize sz
    -> stmt (text "Stack_Size" ==> integer sz <+> text "bytes")
  Priority pri
    -> stmt (text "Priority" ==> integer pri)
  EntryPoint syms
    -> renderEntryPoint syms
  SourceText srcs
    -> renderSrcText srcs
  SendEvents sevs
    -> renderSendsEventsTo sevs
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


