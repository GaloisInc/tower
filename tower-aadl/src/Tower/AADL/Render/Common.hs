--
-- Render helpers.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Common where

import Text.PrettyPrint.Leijen

import           Tower.AADL.AST
import qualified Ivory.Tower.AST.Comment as C
import qualified Ivory.Tower.SrcLoc.Location as L

--------------------------------------------------------------------------------
-- NamesSpaces

typesPkg, sMACCMPkg, dataModelPkg, baseTypesPkg :: String
typesPkg     = "Data_Types"
sMACCMPkg    = "SMACCM_SYS"
dataModelPkg = "Data_Model"
baseTypesPkg = "Base_Types"

-- @ns :: d1@
nameSpace :: Doc -> Doc -> Doc
nameSpace d0 d1 = d0 <> colon <> colon <> d1

fromSMACCM :: Doc -> Doc
fromSMACCM = nameSpace (text sMACCMPkg)

fromBaseTypes :: Doc -> Doc
fromBaseTypes = nameSpace (text baseTypesPkg)

fromTypeDefs :: Doc -> Doc
fromTypeDefs = nameSpace (text typesPkg)

fromDataModel :: Doc -> Doc
fromDataModel = nameSpace (text dataModelPkg)

-- | Imports for all packages
baseImports :: [String]
baseImports =
  [ baseTypesPkg
  , dataModelPkg
  ]

-- | Imports for non-type definition packages. Include typesPkg only if data
-- types defined.
defaultImports :: Bool -> [String]
defaultImports b =
     baseImports
  ++ (if b then [typesPkg] else [])
  ++ [ sMACCMPkg ]

--------------------------------------------------------------------------------
-- Helpers

sendsEventsTo :: Doc
sendsEventsTo = text "Sends_Events_To"

srcText :: Doc
srcText = text "Source_Text"

primSrc :: Doc
primSrc = text "CommPrim_Source_Text"

entrySrc :: Doc
entrySrc = text "Compute_Entrypoint_Source_Text"

initEntryPoint :: Doc
initEntryPoint = text "Initialize_Entrypoint_Source_Text"

mkImpl :: Doc -> Doc
mkImpl d = d <> dot <> text "impl"

tab :: Doc -> Doc
tab = indent 2

stmt :: Doc -> Doc
stmt d = d <> semi

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

txChan :: Doc
txChan = text "Output_"

mkTxChan :: ChanLabel -> Doc
mkTxChan l = txChan <> text l

rxChan :: Doc
rxChan = text "Input_"

mkRxChan :: ChanLabel -> Doc
mkRxChan l = rxChan <> text l

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

-- | Renders a list [foo, bar, ...] as `("foo", "bar", ...)`
renderLs :: [String] -> Doc
renderLs ls =
     lparen
  <> (hsep $ punctuate comma $ map (dquotes . text) ls)
  <> rparen
