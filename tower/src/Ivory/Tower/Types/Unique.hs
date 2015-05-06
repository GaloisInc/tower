module Ivory.Tower.Types.Unique
  ( Unique(..)
  , showUnique
  ) where

data Unique =
  Unique
    { unique_name  :: String
    , unique_fresh :: Integer
    } deriving (Eq, Show, Ord)

showUnique :: Unique -> String
showUnique (Unique n 1) = n
showUnique u = unique_name u ++ "_" ++ show (unique_fresh u)

