module Ivory.Tower.Types.Unique
  ( Unique(..)
  , showUnique
  ) where

data Unique =
  Unique
    { unique_name  :: String
    , unique_fresh :: Integer
    } deriving (Eq, Show, Ord)

-- | Shows a unique value. While this is safe, consider using `uniqueImpl` from
-- Ivory.Tower.Types.Backend to show values in a backend-dependent way.
showUnique :: Unique -> String
showUnique u = unique_name u ++ "_" ++ show (unique_fresh u)

