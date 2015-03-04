--
-- Render AADL source text from the AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render where

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
    , text "connections"
    , tab $ vsep $ map renderConnection $ threadChannels namedThreads
    ]

  namedThreads = zip threads (map (("th"++) . show) [0::Integer ..])
  threads = processComponents p
  nm      = text (processName p)

renderProcessSubcomponent :: (Thread, LocalId) -> Doc
renderProcessSubcomponent (t, var) = stmt $
  text var <> colon <+> text "thread" <+> nameSpace nm nm
  where
  nm = text (threadName t)

renderConnection :: Connection -> Doc
renderConnection c = vsep (allSynchConnect (connectionLabel c))
  where
  synchConnectLabel tx rx = text (tx ++ "_to_" ++ rx)
  synchConnection tx rx i = stmt
      $ synchConnectLabel tx rx
     <> colon
    <+> text "port"
    <+> (text tx <> dot <> mkTxChan i)
    ->> (text rx <> dot <> mkRxChan i)
  allSynchConnect i = do
    tx <- connectionTxLabels c
    rx <- connectionRxLabels c
    return (synchConnection tx rx i)

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
  ChannelFeature c
    -> renderDataPort c

renderDataPort :: Channel -> Doc
renderDataPort c = stmt
    $ mkChan c <> colon
  <+> renderChannelHandle h
  <+> hsep (map text ["event", "data", "port"])
  <+> renderTypeNS (chanType c)
 <$$> tab lbrace
 <$$> tab (tab (vsep st))
 <$$> tab rbrace
  where
  st = renderSourceText (renderChanBody h) (chanCallbacks c)
  h  = chanHandle c

renderChanBody :: ChannelHandle -> Doc
renderChanBody h = case h of
  Input  -> entrySrc
  Output -> primSrc

renderChannelHandle :: ChannelHandle -> Doc
renderChannelHandle h = text $ case h of
  Input  -> "in"
  Output -> "out"

renderSourceText :: Doc -> SourceText -> [Doc]
renderSourceText h st = case st of
  Prim srcTxt
    -> [ stmt $ fromSMACCM h ==> dquotes (text srcTxt) ]
  User srcs
    -> renderEntryText h srcs

renderEntryText :: Doc -> [SourcePath] -> [Doc]
renderEntryText h srcs =
  let (srcTxts, entries) = unzip srcs in
  [ stmt $ fromSMACCM h       ==> mkLs entries
  , stmt $ text "Source_Text" ==> mkLs srcTxts ]
  where
  mkLs s = lparen
        <> hsep (punctuate comma (map (dquotes . text) s))
        <> rparen

renderThreadProperty :: ThreadProperty -> Doc
renderThreadProperty p = case p of
  DispatchProtocol per
    -> mkDisptach per
  ThreadType ty
    -> mkThreadType ty
  ExecTime l h
    -> stmt
     $ text "Compute_Execution_Time"
   ==> prettyTime l <+> dot <> dot <+> prettyTime h
  StackSize sz
    -> stmt (text "Stack_Size" ==> integer sz <+> text "bytes")
  Priority pri
    -> stmt (text "Priority" ==> integer pri)
  PropertySourceText srcTxt
    -> vsep (renderEntryText entrySrc [srcTxt])
  SendEvents chans
    -> stmt
     $ fromSMACCM (text "Sends_Events_To")
   ==> dquotes (braces (braces (mkOuts chans)))
  where
  mkOuts chans = hsep (punctuate comma (map go chans))
    where go (l,m) = integer m <+> mkTxChan l
  mkDisptach per = case per of
    Periodic i
      ->   mkDispatchdec (text "Periodic")
      <$$> stmt (text "Period" ==> prettyTime i)
    Aperiodic
      -> mkDispatchdec (text "Aperiodic")
  mkDispatchdec dis = stmt (text "Dispatch_Protocol" ==> dis)
  mkThreadType ty = stmt (fromSMACCM (text "Thread_Type" ==> renderTy))
    where renderTy = case ty of
                       Passive -> text "Passive"
                       Active  -> text "Active"
