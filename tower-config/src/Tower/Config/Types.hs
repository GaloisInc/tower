{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tower.Config.Types
  ( Value
  , parse
  , Configurable(..)
  , element
  , ppValue
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Text.TOML as T
import           Text.TOML.Value
import qualified Data.Map as M
import           Data.Time.Clock
import           Data.List (intercalate)

parse :: B.ByteString -> Maybe Value
parse b = fmap Left (T.parse b)

element :: Configurable a => String -> Value -> Maybe a
element n v = do
  es <- fromConfig v
  v' <- lookup n es
  fromConfig v'

class Configurable a where
  fromConfig :: Value -> Maybe a

instance Configurable Value where
  fromConfig v = Just v

instance Configurable String where
  fromConfig (Right (VString a)) = Just a
  fromConfig _ = Nothing

instance Configurable Integer where
  fromConfig (Right (VInteger a)) = Just a
  fromConfig _ = Nothing

instance Configurable Double where
  fromConfig (Right (VDouble a)) = Just a
  fromConfig _ = Nothing

instance Configurable Bool where
  fromConfig (Right (VBool a)) = Just a
  fromConfig _ = Nothing

instance Configurable [Value] where
  fromConfig (Right (VArray as)) = Just (map Right as)
  fromConfig _ = Nothing

instance Configurable UTCTime where
  fromConfig (Right (VDate a)) = Just a
  fromConfig _ = Nothing

instance Configurable [(String, Value)] where
  fromConfig (Left (TOML toml)) = Just (M.toList toml)
  fromConfig _ = Nothing


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


