
module Tower.Config.TOML
  ( TOML
  , TOMLV
  , Value
  , ppValue
  , tomlParse
  ) where

import qualified Data.Map as M
import           Data.List (intercalate)
import           Text.TOML.Value
import qualified Text.TOML as T
import qualified Data.ByteString.Char8 as B

ppValue :: Value -> String
ppValue = aux []
  where
  aux ctx (Left (TOML toml)) = header ctx ++ ppBindings ctx (M.toList toml)
  aux _   (Right tomlv) = ppTOMLV tomlv
  ppBindings ctx as = intercalate "\n" $
    map ppBinding (values as) ++ map (ppSubsection ctx) (subsections as)

  ppTOMLV (VString a) = "\"" ++ a ++ "\"" -- XXX not escape safe
  ppTOMLV (VInteger a) = show a
  ppTOMLV (VDouble a) = show a
  ppTOMLV (VBool True) = "true"
  ppTOMLV (VBool False) = "false"
  ppTOMLV (VArray as) = "[ " ++ (intercalate ", " (map ppTOMLV as)) ++ " ]"
  ppTOMLV (VDocument _) = error "Tower.Config.ppValue VDocument not supported"
  ppTOMLV (VDate a) = show a

  header [] = ""
  header sections = "[" ++ (intercalate "." sections) ++ "]\n"

  values [] = []
  values ((s,Right v):as) = (s,v):(values as)
  values ((_,Left  _):as) = values as

  ppBinding (s,v) = s ++ " = " ++ ppTOMLV v

  subsections [] = []
  subsections ((_,Right _):as) = subsections as
  subsections ((s,Left  v):as) = (s,v):(subsections as)

  ppSubsection ctx (s,v) = aux (ctx ++ [s]) (Left v)


tomlParse :: B.ByteString -> Maybe TOML
tomlParse bs = T.parse bs
