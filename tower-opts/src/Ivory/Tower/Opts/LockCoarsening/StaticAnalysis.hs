{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module will go through a Handler and will 
-- return all the external ressources used by this Handler

module Ivory.Tower.Opts.LockCoarsening.StaticAnalysis
  ( staticAnalysisHandler, staticAnalysisMonitor, fromSymToString ) where

import Data.List (nub)

import Ivory.Language()
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax
import Ivory.Language.Syntax.AST
import qualified Ivory.Tower.AST as AST
--import Prelude

fromSymToString :: [Sym] -> [String]
fromSymToString a = a;

staticAnalysisMonitor :: AST.Monitor -> [[Sym]]
staticAnalysisMonitor m = map staticAnalysisHandler $ AST.monitor_handlers m

staticAnalysisHandler :: AST.Handler -> [Sym]
staticAnalysisHandler h = nub $ concat $ map analyseProc (AST.handler_callbacks h)

analyseProc :: Proc -> [Sym]
analyseProc proc = 
  (analyseBlock $ procBody $ proc) ++ 
  (concat $ map analyseRequire $ procRequires proc) ++ 
  (concat $ map analyseEnsure $ procEnsures proc)

analyseBlock :: Block -> [Sym]
analyseBlock block = concat $ map analyseStmt block

analyseRequire :: Require -> [Sym]
analyseRequire = analyseCond . getRequire

analyseEnsure :: Ensure -> [Sym]
analyseEnsure = analyseCond . getEnsure


analyseCond :: Cond -> [Sym]
analyseCond c = case c of
  CondBool e1 -> analyseExpr e1
  CondDeref _ e1 _ c1 -> (analyseExpr e1) ++ (analyseCond c1)

analyseStmt :: Stmt -> [Sym]
analyseStmt stmt = case stmt of
  IfTE e1 b1 b2 -> (analyseExpr e1) ++ (analyseBlock b1) ++ (analyseBlock b2)
    --  If-then-else statement.  The @Expr@ argument will be typed as an IBool

  Assert e1 -> analyseExpr e1
    --  Boolean-valued assertions.  The @Expr@ argument will be typed as an IBool

  CompilerAssert e1 -> analyseExpr e1
    --  Compiler-inserted assertion (as opposed to user-level assertions).
    -- These are expected to be correct (e.g., no overflow, etc).  Not exported.

  Assume e1 -> analyseExpr e1
    --  Boolean-valued assumptions.  The @Expr@ argument will be typed as an
    -- @IBool@.

  Return te1 -> analyseExpr (tValue te1)
    --  Returning a value.

  ReturnVoid -> []
    --  Returning void.

  Deref _ _ e1 -> analyseExpr e1
    --  Reference dereferencing.  The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  Store _ e1 e2 -> analyseExpr e1 ++ (analyseExpr e2)
    --  Storing to a reference.  The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  Assign _ _ e1 -> analyseExpr e1
    --  Simple assignment.

  Call _ _ _ tel -> concat $ map (analyseExpr . tValue) tel
    --  Function call.  The optional variable is where to store the result.  It
    -- is expected that the @Expr@ passed for the function symbol will have the
    -- same type as the combination of the types for the arguments, and the
    -- return type.

  Local _ _ i1 -> analyseInit i1
    --  Stack allocation.  The type parameter is not a reference at this point;
    -- references are allocated separately to the stack-allocated data.

  RefCopy _ e1 e2 -> (analyseExpr e1) ++ (analyseExpr e2)
    --  Ref copy.  Copy the second variable reference to the first (like
    -- memcopy).  The type is the dereferenced value of the variables.

  AllocRef _ _ _ -> []
    --  Reference allocation.  The type parameter is not a reference, but the
    -- referenced type.

  Loop _ _ e1 loopincr b1 -> (analyseExpr e1) ++ (analyseLoopIncr loopincr) ++ (analyseBlock b1)
    --  Looping: arguments are the maximum number of iterations of the loop,
    -- loop variable, start value, break condition (for increment or decrement),
    -- and block.

  Forever b1 -> analyseBlock b1
    --  Nonterminting loop

  Ivory.Language.Syntax.AST.Break -> []
    --  Break out of a loop

  Comment _ -> []
    --  User comment, can be used to output a comment in the backend.

analyseExpr :: Expr -> [Sym]
analyseExpr e = case e of
  ExpSym _ -> []
    --  Symbols

  ExpExtern ext -> [externSym ext]
    --  Imported symbols

  ExpVar _ -> []
    --  Variables

  ExpLit _ -> []
    --  Literals

  ExpLabel _ e1 _ -> analyseExpr e1
    --  Struct label indexing.

  ExpIndex _ e1 _ e2 -> (analyseExpr e1) ++ (analyseExpr e2)
    --  Array indexing.  The type is the type of the array being indexed, it's
    -- implied that the expression with the array in it is a reference.

  ExpToIx e1 _ -> analyseExpr e1
    --  Cast from an expression to an index (Ix) used in loops and array
    -- indexing.  The Integer is the maximum bound.

  ExpSafeCast _ e1 -> analyseExpr e1
    --  Type-safe casting.  The type is the type casted from.

  ExpOp _ le -> concat $ map analyseExpr le
    --  Primitive expression operators

  ExpAddrOfGlobal sym -> [sym]
    --  Take the address of a global memory area, introduced through a MemArea

  ExpMaxMin _ -> []
    --  True is max value, False is min value for the type.

  ExpSizeOf _ -> []
    --  Return the allocation size of the given type.


analyseLoopIncr :: LoopIncr -> [Sym]
analyseLoopIncr li = case li of
  IncrTo e1 -> analyseExpr e1
  DecrTo e1 -> analyseExpr e1

analyseInit :: Ivory.Language.Syntax.AST.Init -> [Sym]
analyseInit i = case i of
  InitZero -> []
  InitExpr _ e1 -> analyseExpr e1
  InitStruct l1 -> concat $ map (analyseInit.snd) $ l1
  InitArray l1 -> concat $ map analyseInit l1