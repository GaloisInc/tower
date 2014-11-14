
module Tower.Config.Parser
  ( ConfigParser()
  , runConfigParser
  , string
  , integer
  , double
  , bool
  , array
  , utctime
  , subsection
  , withDefault
  ) where

import           Control.Applicative
import qualified Data.Map as M
import           Data.Time.Clock
import           Text.TOML.Value
import           Data.Either (lefts, rights)
import           Data.List (intercalate)

newtype ConfigParser a =
  ConfigParser
    { unConfigParser :: Value -> Either String a
    }

instance Functor ConfigParser where
  fmap f c = ConfigParser $ \v -> fmap f (unConfigParser c v)

instance Applicative ConfigParser where
  pure a = ConfigParser (const (pure a))
  (ConfigParser f) <*> (ConfigParser a) = ConfigParser (\v -> (f v) <*> (a v))

instance Monad ConfigParser where
  return = pure
  (ConfigParser a) >>= f = ConfigParser
    (\v -> a v >>= \b -> unConfigParser (f b) v)
  fail e = ConfigParser (const (Left e))

runConfigParser :: ConfigParser a -> Value -> Either String a
runConfigParser = unConfigParser

configParser :: String -> (Value -> Maybe a) -> ConfigParser a
configParser e p = ConfigParser $ \v ->
  case p v of
    Just a -> Right a
    Nothing -> Left ("expected " ++ e ++ ", got " ++ show v)

string :: ConfigParser String
string = configParser "string" $ \v ->
  case v of
    Right (VString a) -> Just a
    _ -> Nothing

integer :: ConfigParser Integer
integer = configParser "integer" $ \v ->
  case v of
    Right (VInteger a) -> Just a
    _ -> Nothing

double :: ConfigParser Double
double = configParser "double" $ \v ->
  case v of
    Right (VDouble a) -> Just a
    _ -> Nothing

bool :: ConfigParser Bool
bool = configParser "bool" $ \v ->
  case v of
    Right (VBool a) -> Just a
    _ -> Nothing

array :: [ConfigParser a] -> ConfigParser [a]
array ps = ConfigParser $ \v ->
  case v of
    Right (VArray as) ->
      let bs = zipWith (\p a -> unConfigParser p (Right a)) ps as
      in case lefts bs of
        [] -> Right (rights bs)
        es -> Left ("got following errors when parsing array elements: " ++ 
                    intercalate "; " es)
    _ -> Left ("expected array, got " ++ show v)

utctime :: ConfigParser UTCTime
utctime = configParser "utctime" $ \v ->
  case v of
    Right (VDate a) -> Just a
    _ -> Nothing

subsection :: String -> ConfigParser a -> ConfigParser a
subsection key vparser = ConfigParser $ \v ->
  case v of
    Left (TOML toml) -> case M.lookup key toml of
      Just v' -> unConfigParser vparser v'
      Nothing -> Left ("failed to find subsection named "
                          ++ key ++ " in " ++ show toml)
    _ -> Left ("expected subsection, got " ++ show v)

withDefault :: ConfigParser a -> a -> ConfigParser a
withDefault p d = ConfigParser $ \v ->
  case unConfigParser p v of
    Right a -> Right a
    Left  _ -> Right d

