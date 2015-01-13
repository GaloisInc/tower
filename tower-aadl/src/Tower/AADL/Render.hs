--
-- Render AADL source text from the AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render where

import Data.Maybe

import Tower.AADL.AST
import Tower.AADL.AST.Common
import Tower.AADL.Render.Common
import Tower.AADL.Render.Types

import qualified Ivory.Tower.AST.Comment as C
import qualified Ivory.Tower.SrcLoc.Location as L

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------
-- Packages

header :: Doc
header = renderStringComment "File generated from Tower-AADL compiler" <$$> empty

mkTowerDoc :: Doc -> Doc
mkTowerDoc doc = header <$$> doc <$$> empty

-- | Place the given construct into a package.
renderPackage :: String -> Doc -> [Import] -> Doc
renderPackage nm doc imports = mkTowerDoc $
        text "package" <+> nm'
   <$$> text "public"
   <$$> vsep (map renderImport imports)
  <$$$> doc
  <$$$> stmt (text "end" <+> nm')
  where nm' = text nm

renderImport :: Import -> Doc
renderImport i = tab (stmt (text "with" <+> text i))

data DocType = TypeDoc | ThreadDoc | SystemDoc
  deriving (Show, Eq)

data CompiledDoc = CompiledDoc
  { docType :: DocType
  , docName :: !String
  , docImpl :: Doc
  } deriving Show

data CompiledDocs = CompiledDocs
  { sysDoc  :: CompiledDoc
  , thdDocs :: [CompiledDoc]
  , tyDoc   :: Maybe CompiledDoc
  } deriving Show

compiledDocs :: CompiledDoc
             -> [CompiledDoc]
             -> Maybe CompiledDoc
             -> CompiledDocs
compiledDocs = CompiledDocs

compiledDoc :: DocType -> String -> Doc -> CompiledDoc
compiledDoc = CompiledDoc

concatDocs :: CompiledDocs -> [CompiledDoc]
concatDocs ds = thdDocs ds ++ (sysDoc ds : maybeToList (tyDoc ds))

compiledTypesDoc :: Doc -> CompiledDoc
compiledTypesDoc = CompiledDoc TypeDoc typesPkg

-- | Render a packaged system, using the extra imports (thread names) if
-- compiling a system.
renderDocPkg :: [Import] -> CompiledDoc -> Doc
renderDocPkg extraImports d =
  renderPackage (docName d) (docImpl d) imps
  where
  imps =
    case docType d of
      TypeDoc
        -> baseImports
      ThreadDoc
        -> defaultImports
      SystemDoc
        -> defaultImports ++ extraImports

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
-- Comments

renderStringComment :: String -> Doc
renderStringComment = renderComment . C.UserComment

renderComment :: C.Comment -> Doc
renderComment c = text "--" <+> cm
  where
  cm = case c of
    C.UserComment s -> text s
    C.SourcePos   s -> renderSrcLoc s

renderSrcLoc :: L.SrcLoc -> Doc
renderSrcLoc s = case s of
  L.NoLoc
    -> text "No source location"
  L.SrcLoc rng msrc
    -> case msrc of
      Nothing  -> renderRng rng
      Just src -> text src <> colon <> renderRng rng

-- Ignore the column.
renderRng :: L.Range -> Doc
renderRng (L.Range (L.Position _ ln0 _) (L.Position _ ln1 _)) =
  if ln0 == ln1
    then int ln0
    else int ln0 <+> char '-' <+> int ln1
