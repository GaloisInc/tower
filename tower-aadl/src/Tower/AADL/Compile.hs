--
-- Data types and helpers for compiled artifacts.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Compile where

import Ivory.Artifact

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

--------------------------------------------------------------------------------
-- Compiled documents

-- | Document type
data DocType = TypeDoc | ThreadDoc | SystemDoc | CodeDoc
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


compiledTypesDoc :: Doc -> CompiledDoc
compiledTypesDoc = CompiledDoc TypeDoc typesPkg

renderCompiledDocs :: CompiledDocs -> [Artifact]
renderCompiledDocs docs = map mkArtifact individual_docs
  where
  mkArtifact :: CompiledDoc -> Artifact
  mkArtifact cdoc = artifactString fname contents
    where
    fname = docName cdoc ++ ".aadl"
    contents = displayS (renderPretty 0.4 100 (renderDocPkg cdoc)) ""

  individual_docs :: [CompiledDoc]
  individual_docs = thdDocs docs ++ (sysDoc docs : maybeToList (tyDoc docs))

  -- | Render a packaged system, using the extra imports (thread names) if
  -- compiling a system.
  renderDocPkg :: CompiledDoc -> Doc
  renderDocPkg d =
    renderPackage (docName d) (docImpl d) imps
    where
    imps = case docType d of
      TypeDoc -> baseImports
      ThreadDoc -> defaultImports (aTypesPkg docs)
      SystemDoc -> defaultImports (aTypesPkg docs) ++ map docName (thdDocs docs)
      CodeDoc -> []

--------------------------------------------------------------------------------
