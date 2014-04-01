module Ivory.Tower.Reporting.Entrypoints
  ( entrypointsDoc
  , entrypointsToFile
  ) where

import qualified Ivory.Tower.AST as AST

-- We'll be inspecting the Ivory AST, which can be unsafe.
import qualified Ivory.Language.Proc as P
import qualified Ivory.Language.Syntax.AST as I

import           Data.Maybe
import           System.IO
import           Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------

-- | Write task entrypoints from a Tower system to a txt file
entrypointsToFile :: FilePath -> String -> AST.System p -> IO ()
entrypointsToFile f nm sysast = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc nm sysast

-- | Write out a .mk file with Makefile variables assigned the task entry
-- points.
entrypointsDoc :: String -> AST.System p -> Doc
entrypointsDoc nm sysast = empty

{-
- vsep
  [ pts "TASKS" (asm_tasks asm)
  , linebreak
  , pts "SIGNALS" (asm_sigs asm)
  ]
  where
  mkVar kind = text nm <> char '_'
            <> text kind <+> equals <+> backslash
  pts _var []    = empty
  pts  var nodes =
    let names = map entryname nodes in
    mkVar var <$$> indent 2
      (vsep (punctuate (empty <+> backslash) $ map text names))

entryname :: AssembledNode a -> String
entryname n = case an_entry n of
  P.DefProc   p -> I.procSym   p
  P.DefExtern p -> I.externSym p
  P.DefImport p -> I.importSym p
-}
--------------------------------------------------------------------------------

