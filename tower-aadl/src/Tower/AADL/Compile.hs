--
-- Data types and helpers for compiled artifacts.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Compile where

import Data.Maybe

import Text.PrettyPrint.Leijen

import Tower.AADL.Render.Common

--------------------------------------------------------------------------------

-- | AADL package imports
type Import = String

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

aTypesPkg :: CompiledDocs -> Bool
aTypesPkg = isJust . tyDoc

concatDocs :: CompiledDocs -> [CompiledDoc]
concatDocs ds = thdDocs ds ++ (sysDoc ds : maybeToList (tyDoc ds))

compiledTypesDoc :: Doc -> CompiledDoc
compiledTypesDoc = CompiledDoc TypeDoc typesPkg

-- | Render a packaged system, using the extra imports (thread names) if
-- compiling a system.
renderDocPkg :: Bool -> [Import] -> CompiledDoc -> Doc
renderDocPkg b extraImports d =
  renderPackage (docName d) (docImpl d) imps
  where
  imps =
    case docType d of
      TypeDoc
        -> baseImports
      ThreadDoc
        -> defaultImports b
      SystemDoc
        -> defaultImports b ++ extraImports
