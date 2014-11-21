--
-- Render AADL source text from the AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render where

import Tower.AADL.AST.AST
import Tower.AADL.AST.ProcessAST

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------

renderPackage :: Package -> Doc
renderPackage p =
        text "package" <+> text (packageName p)
   <$$> text "public"
   <$$> vsep (map renderImport (packageImports p))
  <$$$> vsep (map renderSystem (packageSystem p))

renderImport :: String -> Doc
renderImport i = tab (stmt (text "with" <+> text i))

renderSystem :: System -> Doc
renderSystem s =
        vsep (map renderProcess (systemComponents s))
  <$$$> renderBlk (text "system") nm []
  <$$$> renderBlk (text "system" <+> text "implementation") (mkImpl nm) blk
  where
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
        skipLines (map renderThread threads)
  <$$$> renderBlk (text "process") nm []
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
  impl    = mkImpl nm
  def     = text "process"
    <+> text "implementation"
    <+> impl

renderProcessSubcomponent :: (Thread, LocalId) -> Doc
renderProcessSubcomponent (t, var) = stmt $
  text var <> colon <+> text "thread" <+> text (threadName t)

renderConnection :: Connection -> Doc
renderConnection c = vsep (allSynchConnect (connectionLabel c))
  where
  synchConnectLabel tx rx = text (tx ++ "_to_" ++ rx)
  synchConnection tx rx i = stmt
      $ synchConnectLabel tx rx
     <> colon
    <+> text "port"
    <+> text tx <> dot <> mkTxChan i
    <+> char '-' <> rangle
    <+> text rx <> dot <> mkRxChan i
  allSynchConnect i = do
    tx <- connectionTxLabels c
    rx <- connectionRxLabels c
    return (synchConnection tx rx i)

renderThread :: Thread -> Doc
renderThread t =
  renderBlk (text "thread") (text (threadName t))
    [ (text "features")
    , tab $ vsep $ map renderThreadFeature $ threadFeatures t
    , (text "properties")
    , tab $ vsep $ map renderThreadProperty $ threadProperties t
    ]

renderThreadFeature :: Feature -> Doc
renderThreadFeature f = case f of
  ChannelFeature c
    -> renderDataPort c

renderDataPort :: Channel -> Doc
renderDataPort c = stmt
    $ mkChan c <> colon
  <+> fst inout
  <+> hsep (map text ["event", "data", "port"])
 <$$> tab lbrace
 <$$> tab (tab (vsep st))
 <$$> tab rbrace
  where
  st = renderSourceText (snd inout) (chanCallbacks c)
  h = chanHandle c
  inout = case h of
    Input ->  (text "in", entrySrc)
    Output -> (text "out", primSrc)

-- | Takes the kind of source text (e.g. "Compute_Entrypoint_Source_Text" or
-- "CommPrim_Source_Text"), source text, and makes the property.
renderSourceText :: Doc -> SourceText -> [Doc]
renderSourceText h st = case st of
  Prim fns
    -> [ mkSrcText fns ]
  User srcs
    ->   let (fps, fns) = unzip srcs in
         [ mkSrcText fns
         , stmt (text "Source_Text" ==> mkLs fps) ]
  where
  mkSrcText fns = stmt $ fromSMACCM h ==> mkLs fns
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
  PropertySourceText st
    -> vsep (renderSourceText entrySrc st)
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

--------------------------------------------------------------------------------
-- Helpers

primSrc :: Doc
primSrc = text "CommPrim_Source_Text"
entrySrc :: Doc
entrySrc = text "Compute_Entrypoint_Source_Text"

mkImpl :: Doc -> Doc
mkImpl d = d <> dot <> text "impl"

tab :: Doc -> Doc
tab = indent 2

stmt :: Doc -> Doc
stmt d = d <> semi

nameSpace :: Doc -> Doc -> Doc
nameSpace d0 d1 = d0 <> colon <> colon <> d1

fromSMACCM :: Doc -> Doc
fromSMACCM = nameSpace (text "SMACCM_SYS")

(==>) :: Doc -> Doc -> Doc
(==>) d0 d1 = d0 <+> equals <> rangle <+> d1

(->>) :: Doc -> Doc -> Doc
(->>) d0 d1 = d0 <+> char '-' <> rangle <+> d1

-- | Skip a line.
(<$$$>) :: Doc -> Doc -> Doc
(<$$$>) d0 d1 = d0 <$$> empty <$$> d1

-- | Separate with line breaks.
skipLines :: [Doc] -> Doc
skipLines = vsep . (punctuate linebreak)

mkTxChan :: String -> Doc
mkTxChan l = text "Output" <> text l

mkRxChan :: String -> Doc
mkRxChan l = text "Input" <> text l

mkChan :: Channel -> Doc
mkChan c =
  let l = chanLabel c in
  case chanHandle c of
    Input  -> mkRxChan l
    Output -> mkTxChan l

-- | Takes the kind of block, block name, statements (e.g., features/properties) etc.
renderBlk :: Doc -> Doc -> [Doc] -> Doc
renderBlk kind nm stmts =
       kind <+> nm
  <$$> tab (vsep stmts)
  <$$> stmt (text "end" <+> nm)

prettyTime :: Integer -> Doc
prettyTime i = t
  where
  t = case i `mod` 1000 of
    0 -> integer (i `div` 1000) <+> text "ms"
    _ -> integer i <+> text "us"
