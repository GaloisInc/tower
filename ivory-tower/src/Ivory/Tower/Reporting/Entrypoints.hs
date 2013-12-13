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
import Data.Char

--------------------------------------------------------------------------------

-- | Write task entrypoints from a Tower 'Assembly' to a txt file
entrypointsToFile :: FilePath -> String -> Assembly -> IO ()
entrypointsToFile f nm asm = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc nm asm

entrypointsDoc :: String -> Assembly -> Doc
entrypointsDoc nm asm = vsep
  [ mkVar "TASKS"
  , pts (asm_tasks asm)
  , linebreak
  , mkVar "SIGNALS"
  , pts (asm_sigs asm)
  ]
  where
  mkVar kind = text (map toUpper nm) <> char '_' <> text kind <+> equals <+> backslash
  pts nodes =
    let names = map entryname nodes in
    indent 2 $ vsep (punctuate (empty <+> backslash) $ map text names)

entryname :: AssembledNode a -> String
entryname n = case an_entry n of
  P.DefProc   p -> AST.procSym   p
  P.DefExtern p -> AST.externSym p
  P.DefImport p -> AST.importSym p


