module Ivory.Tower.Types.Unique
  ( Unique(..)
  , showUnique
  ) where

data Unique =
  Unique
    { unique_name  :: String
    , unique_fresh :: Integer
    }

showUnique :: Unique -> String
showUnique u = unique_name u ++ "_" ++ show (unique_fresh u)

