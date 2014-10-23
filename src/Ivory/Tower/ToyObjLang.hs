{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.ToyObjLang
  ( module Ivory.Language
  ) where
import Ivory.Language
{-

  ( Module()
  , package
  , defProc
  , defVar
  , ModuleM()
  , Var()
  , var
  , Proc()
  , proc
  , ProcM()
  , stmt
  , call
  , comment

  , display
  , printModules
  ) where

import Control.Applicative (Applicative)
import MonadLib
import Text.PrettyPrint.Mainland

data Proc = Proc String [String] [String]
  deriving (Eq, Show)
data Var = Var String
  deriving (Eq, Show)

data Module =
  Module
    { mod_name :: String
    , mod_vars :: [Var]
    , mod_procs :: [Proc]
    }

ppModule :: Module -> Doc
ppModule m = stack
  ((text "module" <+> text (mod_name m))
   : ((map (indent 4 . ppVar) (mod_vars m))
      ++ (map (indent 4 . ppProc) (mod_procs m))))

ppProc :: Proc -> Doc
ppProc (Proc n as bs) = stack
  ((text "proc" <+> text n <+> parens (commasep (map text as)))
   :(map (indent 4 . text) bs))

ppVar :: Var -> Doc
ppVar (Var n) = text "var" <+> text n

display :: Module -> String
display m = pretty 80 (ppModule m)

newtype ModuleM a = ModuleM
  { unModuleM :: StateT Module Id a
  } deriving (Functor, Applicative, Monad)

withModule :: (Module -> Module) -> ModuleM ()
withModule f = ModuleM $ do
  m <- get
  set (f m)

var :: String -> Var
var name = Var name

defVar :: Var -> ModuleM ()
defVar v = withModule $
  \m -> m { mod_vars = v : (mod_vars m) }

proc :: String -> [String] -> ProcM () -> Proc
proc name args b = snd (runM (unProcM b) (Proc name args []))

defProc :: Proc -> ModuleM ()
defProc p = withModule $
  \m -> m { mod_procs = p : (mod_procs m) }

newtype ProcM a = ProcM
  { unProcM :: StateT Proc Id a
  } deriving (Functor, Applicative, Monad)

withProc :: (Proc -> Proc) -> ProcM ()
withProc f = ProcM $ do
  m <- get
  set (f m)

stmt :: String -> ProcM ()
stmt s = withProc $ \(Proc n as bs) -> Proc n as (bs ++ [s])

comment :: String -> ProcM ()
comment s = stmt ("/* " ++ s ++ " */")

call :: Proc -> ProcM ()
call (Proc name _ _) = stmt ("call " ++ name)

package :: String -> ModuleM () -> Module
package n b = snd (runM (unModuleM b) (Module n [] []))

printModules :: [Module] -> IO ()
printModules = putStrLn . pretty 80 . stack . dblspace . map ppModule
  where
  dblspace = punctuate (empty </> empty)

_objlangtest :: IO ()
_objlangtest = printModules
  [ package "emptypackage" (return ())
  , package "singlevar" $ do
      defVar $ var "foo"
  , package "singleproc" $ do
      defProc $ proc "bar" ["baz", "bim"] $ do
        stmt "first statement"
        stmt "second statement"
  ]
-}
