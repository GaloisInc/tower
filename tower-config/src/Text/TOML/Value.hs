{-# LANGUAGE CPP #-}
module Text.TOML.Value
  ( TOML (..)
  , TOMLV (..)
  , Value
  , tempty
  , tinsert
  , liftT
  , liftTV
  )where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Time.Clock

#if __GLASGOW_HASKELL__ < 708
import System.Locale ()
import Data.Time.Format
#else
import System.Locale ()
import Data.Time.Format ()
#endif

type Value = Either TOML TOMLV

newtype TOML = TOML (Map String Value)
    deriving ( Eq, Ord, Show )

data TOMLV
    = VString String
    | VInteger Integer
    | VDouble Double
    | VBool Bool
    | VArray [TOMLV]
    | VDocument TOML
    | VDate UTCTime
    deriving ( Eq, Ord, Show )

tempty = TOML M.empty

liftT :: (Map String Value -> Map String Value) -> TOML -> TOML
liftT f (TOML m) = (TOML $ f m)

liftTV :: (TOML -> TOML) -> Value -> Value
liftTV f (Left  t) = Left $ f t
liftTV f (Right _) = Left $ f tempty

tinsert :: String -> Value -> TOML -> TOML
tinsert k v t = liftT (M.insert k v) t
