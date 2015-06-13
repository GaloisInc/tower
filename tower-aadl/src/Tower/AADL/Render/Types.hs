--
-- Render AADL source text from Ivory data types.  Compound types are always
-- placed in their own package, TYPES.aadl.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Types
  ( NS(..)
  , renderTypeNS
  , defType
  , defineTypes
  ) where

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
  I.TyIndex _           -> True
  _                     -> False

-- | A defined (compound) type?
defType :: I.Type -> Bool
defType ty = case ty of
  I.TyStruct{}          -> True
  I.TyArr{}             -> True
  I.TyRef t             -> defType t
  _                     -> False

-- What namespace is the caller in?
data NS =
    Base
  | Types
  | Other
  deriving (Show, Read, Eq)

-- | Render the *use* of a type, and namespace if true.
renderTypeNS :: NS -> I.Type -> Doc
renderTypeNS ns ty
  | baseType ty = if ns == Base  then d else fromBaseTypes d
  | defType  ty = case ty of
                    I.TyStruct{}
                      -> fromTypeDefs d
                    _ -> if ns == Types then di else fromTypeDefs di
  | otherwise   = tyError ty
  where
  d  = renderType ty
  di = mkImpl d

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

-- | Define types. We treat structs specially since we need than their
-- implementation to define them.
defineTypes :: [I.Type] -> [(FilePath, I.Struct)] -> Doc
defineTypes tys strs = vcat $ map go tys'
  where
  go ty =
    case ty of
      I.TyRef t    -> go t
      I.TyStruct n -> renderStruct (structImpl strs n)
      I.TyArr sz t -> renderArray sz t
      _            -> empty

  -- These are all the types, recursively expanded, in the dependency order.
  tys' :: [I.Type]
  tys' = nub (concatMap flatten tys)
  flatten ty =
    case ty of
      I.TyRef t    -> flatten t
      I.TyStruct n ->
        case snd $ structImpl strs n of
          I.Struct _nm fields
            -> concatMap (flatten . I.tType) fields ++ [ty]
          _ -> [ty]
      I.TyArr _sz t -> flatten t ++ [ty]
      _             -> []

--------------------------------------------------------------------------------
-- Define structures

renderStruct :: (FilePath, I.Struct) -> Doc
renderStruct (_hdr, I.Abstract nm path) =
  error $ "Abstract struct " ++ nm
       ++ " on path " ++ show path ++ " can't be generated."
renderStruct (hdr, I.Struct nm _fields) = renderCompoundTy nm' body
  where
  nm' = text nm
  body = tab (text "properties")
    <$$> (tab $ tab $ external)
    <$$> (tab $ tab $ stmt
              $     fromSMACCM (text "CommPrim_Source_Header")
                ==> (dquotes (text hdr)))

structImpl :: [(FilePath, I.Struct)] -> String -> (FilePath, I.Struct)
structImpl structs nm =
  case find ((nm ==) . I.structName . snd) structs of
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
    <$$> dm (text "Base_Type") renderClassifier
    <$$> dm (text "Dimension") (parens (int sz))
  renderClassifier =
    -- Only worry about BaseTypes namespace, since all defined types go in the
    -- same package.
    parens (    text "classifier"
            <+> parens t
           )
  t = renderTypeNS Types ty
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
