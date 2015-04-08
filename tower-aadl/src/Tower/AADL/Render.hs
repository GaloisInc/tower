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
import Data.Maybe (catMaybes)

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
 <$$> chanSrc (vsep st)
  where
  st = renderEntryText [inputCallback rx]

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

renderEntryText :: [SourcePath] -> [Doc]
renderEntryText srcs =
  stmt (fromSMACCM entrySrc ==> mkLs cbs) : [stext]

  where
  (mfps, cbs) = unzip srcs
  stext =
    let fps = catMaybes mfps in
    if null fps then empty
      else stmt $ text "Source_Text"  ==> mkLs fps
  mkLs ss = lparen
         <> dquotes (vsep (punctuate comma (map text ss)))
         <> rparen

renderThreadProperty :: ThreadProperty -> Doc
renderThreadProperty p = case p of
  DispatchProtocol per
    -> mkDisptach per
  ThreadType ty
    -> mkThreadType ty
  External
    -> stmt $ fromSMACCM (text "IsExternal") ==> text "true"
  ExecTime l h
    -> stmt
     $ text "Compute_Execution_Time"
   ==> prettyTime l <+> dot <> dot <+> prettyTime h
  StackSize sz
    -> stmt (text "Stack_Size" ==> integer sz <+> text "bytes")
  Priority pri
    -> stmt (text "Priority" ==> integer pri)
  PropertySourceText srcTxts
    -> vsep (renderEntryText srcTxts)
  SendEvents txs
    -> stmt
     $ fromSMACCM (text "Sends_Events_To")
   ==> dquotes (braces (braces (mkOuts txs)))
  where
  mkOuts txs = hsep (punctuate comma (map go txs))
    where go (tx,bnd) = integer bnd <+> mkTxChan (outputLabel tx)
  mkDisptach per = case per of
    Periodic i
      ->   mkDispatchdec (text "Periodic")
      <$$> stmt (text "Period" ==> prettyTime i)
    Aperiodic
      -> mkDispatchdec (text "Sporadic")
  mkDispatchdec dis = stmt (text "Dispatch_Protocol" ==> dis)
  mkThreadType ty = stmt (fromSMACCM (text "Thread_Type" ==> renderTy))
    where renderTy = case ty of
                       Passive -> text "Passive"
                       Active  -> text "Active"
