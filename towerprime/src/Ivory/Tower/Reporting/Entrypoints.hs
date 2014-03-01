module Ivory.Tower.Reporting.Entrypoints
  ( entrypointsDoc
  , entrypointsToFile
  , entrypointsToXML
  ) where

import Ivory.Tower.Types.Assembly

-- We'll be inspecting the Ivory AST, which can be unsafe.
import qualified Ivory.Language.Proc as P
import qualified Ivory.Language.Syntax.AST as AST

import           Data.Maybe
import           System.IO
import           Text.PrettyPrint.Leijen
import qualified Text.XML.Light as X

--------------------------------------------------------------------------------

-- | Write task entrypoints from a Tower 'Assembly' to a txt file
entrypointsToFile :: FilePath -> String -> Assembly -> IO ()
entrypointsToFile f nm asm = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc nm asm

-- | Write out a .mk file with Makefile variables assigned the task entry
-- points.
entrypointsDoc :: String -> Assembly -> Doc
entrypointsDoc nm asm = empty 

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
  P.DefProc   p -> AST.procSym   p
  P.DefExtern p -> AST.externSym p
  P.DefImport p -> AST.importSym p
-}
--------------------------------------------------------------------------------

entrypointsToXML :: FilePath -> Assembly -> IO ()
entrypointsToXML f asm = writeFile f "garbage" 
{- (unlines xmlOut)
  where
  xmlOut = map mkXML (asm_tasks asm)
  mkXML node = X.ppcElement X.defaultConfigPP $ X.node (mkName "taskdata") rst
    where
    rst :: [X.Attr]
    rst = [ X.Attr (mkName "taskname")
                   (entryname node)
          , X.Attr (mkName "stacksize")
                   (show $ taskst_stacksize taskSt)
          , X.Attr (mkName "priority")
                   (show $ fromMaybe 0 $ taskst_priority taskSt)
          ]

    taskSt = nodest_impl (an_nodest node)

    mkName :: String -> X.QName
    mkName nm = X.QName nm Nothing Nothing
-}
--------------------------------------------------------------------------------
