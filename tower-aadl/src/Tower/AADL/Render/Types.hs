--
-- Render AADL source text from Ivory data types.  Compound types are always
-- placed in their own package, TYPES.aadl.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Types where

import Data.List

import Text.PrettyPrint.Leijen

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Array       as I

import Tower.AADL.Render.Common

--------------------------------------------------------------------------------

-- | A base type?
baseType :: I.Type -> Bool
baseType ty = case ty of
  I.TyVoid              -> True
  I.TyChar              -> True
  I.TyInt{}             -> True
  I.TyWord{}            -> True
  I.TyBool              -> True
  I.TyFloat             -> True
  I.TyDouble            -> True
  I.TyRef t             -> baseType t
  _                     -> False

-- | A defined (compound) type?
defType :: I.Type -> Bool
defType ty = case ty of
  I.TyStruct{}          -> True
  I.TyArr{}             -> True
  I.TyRef t             -> defType t
  _                     -> False

-- | Render the *use* of a type, including namespace.
renderTypeNS :: I.Type -> Doc
renderTypeNS ty
  | baseType ty = fromBaseTypes d
  | defType  ty = mkImpl (fromTypeDefs d)
  | otherwise   = tyError ty
  where
  d = renderType ty

-- | Render the *use* of a type.
renderType :: I.Type -> Doc
renderType ty = case ty of
  I.TyVoid              -> text "Void"
  I.TyChar              -> text "Char"
  I.TyInt i             -> text (intSize i)
  I.TyWord w            -> text (wordSize w)
  I.TyBool              -> text "Boolean"
  I.TyFloat             -> text "Float"
  I.TyDouble            -> text "Double"
  I.TyIndex _           -> renderType I.ixRep
  I.TyRef t             -> renderType t  -- LOSSY
  I.TyStruct n          -> text n
  I.TyArr sz t          -> arrayName sz t

  I.TyConstRef _t       -> tyError ty
  I.TyPtr _t            -> tyError ty
  I.TyCArray _t         -> tyError ty
  I.TyProc _retT _argTs -> tyError ty
  I.TyOpaque            -> tyError ty
  where
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

--------------------------------------------------------------------------------

-- | Define types. We treat structs specially since we need more than their
-- implementation to define them.
defineTypes :: [I.Type] -> [I.Struct] -> Doc
defineTypes tys strs = vcat (map go tys)
  where
  go ty =
    case ty of
      I.TyRef t    -> go t
      I.TyStruct n -> renderStruct (structImpl strs n)
      I.TyArr sz t -> renderArray sz t
      _            -> empty

--------------------------------------------------------------------------------
-- Define structures

renderStruct :: I.Struct -> Doc
renderStruct (I.Abstract nm path) =
  error $ "Abstract struct " ++ nm
       ++ " on path " ++ show path ++ " can't be generated."
renderStruct (I.Struct nm fields) =
        renderCompoundTy nm' props
  <$$$> renderStructImp nm' fields
  where
  nm' = text nm
  props = tab (text "properties")
     <$$> renderDataRep "Struct"

renderStructImp :: Doc -> [I.Typed String] -> Doc
renderStructImp nm fields = renderCompoundTyImp nm body
  where
  body = tab (text "subcomponents")
    <$$> tab (tab (vcat (map renderField fields)))
  renderField :: I.Typed String -> Doc
  renderField field = stmt
                    $ text (I.tValue field)
                  <+> colon
                  <+> text "data"
                  <+> renderTypeNS (I.tType field)

structImpl :: [I.Struct] -> String -> I.Struct
structImpl structs nm =
  case find ((nm ==) . I.structName) structs of
    Nothing
      -> error $ "Struct type "
           ++ nm
           ++ " does not have a corresponding implementation passed in."
    Just s
      -> s

--------------------------------------------------------------------------------
-- Define arrays

renderArray :: Int -> I.Type -> Doc
renderArray sz ty =
        renderCompoundTy nm empty
  <$$$> renderCompoundTyImp nm blk
  where
  -- Construct names by showing len and showing the type. If these names
  -- become unweildy, we can add a state monad to add fresh names.
  nm = arrayName sz ty
  blk =  tab (text "properties")
    <$$> renderDataRep "Array"
    <$$> dm (text "BaseType") renderClassifier
    <$$> dm (text "Dimension") (parens (int sz))
  renderClassifier =
    -- Only worry about BaseTypes namespace, since all defined types go in the
    -- same package.
    parens (    text "classifier"
            <+> parens (if baseType ty then fromBaseTypes t else t)
           )
  t = renderType ty
  dm doc res =
    tab $ tab $ stmt (fromDataModel doc  ==> res)

arrayName :: Int -> I.Type -> Doc
arrayName sz ty = text "array_" <> int sz <> text "_" <> renderType ty

--------------------------------------------------------------------------------

renderCompoundTy :: Doc -> Doc -> Doc
renderCompoundTy nm blk =
       text "data" <+> nm
  <$$> blk
  <$$> stmt (text "end" <+> nm)

renderCompoundTyImp :: Doc -> Doc -> Doc
renderCompoundTyImp nm blk =
       text "data"
   <+> text "implementation"
   <+> impl
  <$$> blk
  <$$> stmt (text "end" <+> impl)
  where
  impl = mkImpl nm

renderDataRep :: String -> Doc
renderDataRep t =
    tab
  $ tab
  $ stmt
  $ fromDataModel (text "Data_Representation") ==> (text t)

tyError :: I.Type -> a
tyError ty = error $ "cannot translate TyConstRef" ++ show ty
