
module Ivory.Tower.Reporting.Entrypoints
  ( entrypointsDoc
  , entrypointsToFile
  ) where

import Ivory.Tower.Types

-- We'll be inspecting the Ivory AST, which can be unsafe.
import qualified Ivory.Language.Proc as P
import qualified Ivory.Language.Syntax.AST as AST

import System.IO
import Text.PrettyPrint.Leijen

-- | Write task entrypoints from a Tower 'Assembly' to a txt file
entrypointsToFile :: FilePath -> Assembly -> IO ()
entrypointsToFile f asm = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc asm

entrypointsDoc :: Assembly -> Doc
entrypointsDoc asm = vsep
  [ text "# tasks:"
  , vsep [ text (entryname node) | node <- asm_tasks asm ]
  , text "# signals:"
  , vsep [ text (entryname node) | node <- asm_sigs asm ]
  ]

entryname :: AssembledNode a -> String
entryname n = case an_entry n of
  P.DefProc   p -> AST.procSym   p
  P.DefExtern p -> AST.externSym p
  P.DefImport p -> AST.importSym p


