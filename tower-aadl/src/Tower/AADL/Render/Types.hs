--
-- Render AADL source text from Ivory data types.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Types where

import Text.PrettyPrint.Leijen

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Array       as I

import Tower.AADL.Render.Common

--------------------------------------------------------------------------------

-- | Just render the use of a type. The only "tricky" part is that we place
-- compound data types in their own namespace.
renderType :: I.Type -> Doc
renderType ty = case ty of
    I.TyVoid              -> basetype "Void"
    I.TyChar              -> basetype "Char"
    I.TyInt i             -> basetype (intSize i)
    I.TyWord w            -> basetype (wordSize w)
    I.TyIndex _           -> renderType I.ixRep
    I.TyBool              -> basetype "Boolean"
    I.TyFloat             -> basetype "Float"
    I.TyDouble            -> basetype "Double"
    I.TyStruct n          -> fromTypeDefs (text n)
    I.TyRef t             -> renderType t  -- LOSSY
    I.TyArr len t         -> arrayType len t
    I.TyConstRef _t       -> error "cannot translate TyConstRef"
    I.TyPtr _t            -> error "cannot translate TyPtr"
    I.TyCArray _t         -> error "cannot translate TyCArray"
    I.TyProc _retT _argTs -> error "cannot translate TyProc"
    I.TyOpaque            -> error "cannot translate TyOpaque"
  where
  basetype :: String -> Doc
  basetype = fromBaseTypes . text

  intSize :: I.IntSize -> String
  intSize I.Int8  = "Integer_8"
  intSize I.Int16 = "Integer_16"
  intSize I.Int32 = "Integer_32"
  intSize I.Int64 = "Integer_64"

  wordSize :: I.WordSize -> String
  wordSize I.Word8  = "Unsigned_8"
  wordSize I.Word16 = "Unsigned_16"
  wordSize I.Word32 = "Unsigned_32"
  wordSize I.Word64 = "Unsigned_64"

arrayType :: Int -> I.Type -> Doc
arrayType len basetype = error "arrays not implemented"

--------------------------------------------------------------------------------
-- Define structures

renderStruct :: I.Struct -> Doc
renderStruct (I.Abstract nm path) =
  error $ "Abstract struct " ++ nm
       ++ " on path " ++ show path ++ " can't be generated."
renderStruct (I.Struct nm fields) =
        renderCompoundTy nm props
  <$$$> renderStructImp nm fields
  where
  props = tab (text "properties") <$$> renderDataRep "Struct"

renderStructImp :: String -> [I.Typed String] -> Doc
renderStructImp nm fields = renderCompoundTyImp nm body
  where
  body = tab (text "subcomponents")
    <$$> tab (tab (vcat (map renderField fields)))
  renderField :: I.Typed String -> Doc
  renderField field = stmt
                    $ text (I.tValue field)
                  <+> colon
                  <+> text "data"
                  <+> renderType (I.tType field)

--------------------------------------------------------------------------------
-- Define arrays

-- XXX must generate fresh names for each kind of array
renderArray :: Int -> I.Type -> Doc
renderArray sz ty =
        renderCompoundTy nm empty
  <$$$> renderCompoundTyImp nm blk
  where
  -- XXX Construct names by showing len and showing the type. If these names
  -- become unweildy, we can add a state monad to add fresh names.
  nm = arrayName sz ty
  blk =  text "properties"
    <$$> renderDataRep "Array"
    -- XXX
    <$$> stmt (fromDataModel (text "BaseType")  ==> renderClassifier)
    <$$> stmt (fromDataModel (text "Dimension") ==> parens (int sz))
  renderClassifier =
    -- XXX
    parens (text "classifier" <+> parens (fromBaseTypes (renderType ty)))

-- What else besides "Base_Type"?
-- Data_Model::Base_Type => (classifier (Base_Types::Unsigned_32));

arrayName :: Int -> I.Type -> String
arrayName sz ty = "array_" ++ show sz ++ "_" ++ show ty

--------------------------------------------------------------------------------

renderCompoundTy :: String -> Doc -> Doc
renderCompoundTy nm blk =
       text "data" <+> text nm
  <$$> blk
  <$$> stmt (text "end" <+> text nm)

renderCompoundTyImp :: String -> Doc -> Doc
renderCompoundTyImp nm blk =
       text "data"
   <+> text "implementation"
   <+> impl
  <$$> blk
  <$$> stmt (text "end" <+> impl)
  where
  impl = mkImpl (text nm)

renderDataRep :: String -> Doc
renderDataRep t =
    tab
  $ tab
  $ stmt
  $ fromDataModel (text "Data_Representation") ==> (text t)
