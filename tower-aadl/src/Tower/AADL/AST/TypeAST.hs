--
-- AST for Ivory types and data declarations.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST.TypeAST where

-- XXX
import Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

data ChanType = ChanType
  deriving (Show, Eq)

renderChanType :: I.Type -> ChanType
renderChanType ty = ChanType -- XXX
