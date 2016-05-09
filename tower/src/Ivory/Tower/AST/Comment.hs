{-# LANGUAGE OverloadedStrings #-}
--
-- User and source location comments.
--
-- (c) 2014 Galois, Inc.
--

module Ivory.Tower.AST.Comment where

import Text.PrettyPrint.Mainland

import Ivory.Tower.SrcLoc.Location

--------------------------------------------------------------------------------

data Comment = UserComment String
             | SourcePos   SrcLoc
               deriving (Show, Eq, Ord)

ppSrcLoc :: SrcLoc -> Doc
ppSrcLoc s = case s of
  NoLoc
    -> text "No source location"
  SrcLoc rng msrc
    -> case msrc of
      Nothing  -> ppRng rng
      Just src -> text src <> colon <> ppRng rng

-- Ignore the column.
ppRng :: Range -> Doc
ppRng (Range (Position _ ln0 _) (Position _ ln1 _)) =
  if ln0 == ln1
    then text (show ln0)
    else text (show ln0) <+> char '-' <+> text (show ln1)

instance Pretty Comment where
  ppr (UserComment s) = enclose "/*" "*/" (text s)
  ppr (SourcePos sl) = ppSrcLoc sl
