--
-- Render helpers.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Common where

import Text.PrettyPrint.Leijen

import Tower.AADL.AST

--------------------------------------------------------------------------------
-- NamesSpaces

typesPkg, sMACCMPkg, dataModelPkg, baseTypesPkg :: String
typesPkg     = "TYPES"
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

-- | Imports for non-type definition packages
defaultImports :: [String]
defaultImports =
  baseImports ++
  [ typesPkg
  , sMACCMPkg
  ]

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
