--
-- Render AADL source text from Ivory data types.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Render.Types where

import Text.PrettyPrint.Leijen

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Array as I

import Tower.AADL.Render.Common

--------------------------------------------------------------------------------

renderType :: I.Type -> Doc
renderType ty = case ty of
    I.TyVoid              -> basetype "Void"
    I.TyChar              -> basetype "Char"
    I.TyInt i             -> intSize i
    I.TyWord w            -> wordSize w
    I.TyIndex _           -> renderType I.ixRep
    I.TyBool              -> basetype "Boolean"
    I.TyFloat             -> basetype "Float"
    I.TyDouble            -> basetype "Double"
    I.TyStruct n          -> fromTypeDefs (text n)
    I.TyConstRef _t       -> error "cannot translate TyConstRef"
    I.TyRef t             -> renderType t  -- LOSSY
    I.TyPtr _t            -> error "cannot translate TyPtr"
    I.TyArr len t         -> arrayType len t
    I.TyCArray _t         -> error "cannot translate TyCArray"
    I.TyProc _retT _argTs -> error "cannot translate TyProc"
    I.TyOpaque            -> error "cannot translate TyOpaque"
  where
  basetype :: String -> Doc
  basetype = fromBaseTypes . text

  intSize :: I.IntSize -> Doc
  intSize I.Int8  = basetype "Integer_8"
  intSize I.Int16 = basetype "Integer_16"
  intSize I.Int32 = basetype "Integer_32"
  intSize I.Int64 = basetype "Integer_64"

  wordSize :: I.WordSize -> Doc
  wordSize I.Word8  = basetype "Unsigned_8"
  wordSize I.Word16 = basetype "Unsigned_16"
  wordSize I.Word32 = basetype "Unsigned_32"
  wordSize I.Word64 = basetype "Unsigned_64"

arrayType :: Int -> I.Type -> Doc
arrayType len basetype = error "arrays not implemented"

  -- n <- docName `fmap` getTypeCtx
  -- writeImport n
  -- writeTypeCtxDefinition dtarray
  -- return $ QualTypeName n arraytn
  -- where
  -- dtarray = DTArray arraytn len basetype
  -- arraytn = identifier $ arrayTypeNameS len basetype
