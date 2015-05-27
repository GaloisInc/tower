
module Ivory.Tower.Config.Parser
  ( ConfigParser()
  , runConfigParser
  , string
  , integer
  , double
  , bool
  , array
  , subsection
  , withDefault
  , (<|>)
  , (<?>)
  ) where

import           Control.Applicative hiding ((<|>))
import qualified Data.Map as M
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

array :: ConfigParser a -> ConfigParser [a]
array p = ConfigParser $ \v ->
  case v of
    Right (VArray as) ->
      let bs = map (\a -> unConfigParser p (Right a)) as
      in case lefts bs of
        [] -> Right (rights bs)
        es -> Left ("got following errors when parsing array elements: " ++ 
                    intercalate "; " es)
    _ -> Left ("expected array, got " ++ show v)

subsection :: String -> ConfigParser a -> ConfigParser a
subsection key vparser = ConfigParser $ \v ->
  case v of
    Left (TOML toml) -> case M.lookup key toml of
      Just v' -> unConfigParser vparser v'
      Nothing -> Left ("failed to find subsection named "
                          ++ key ++ " in " ++ show toml)
    _ -> Left ("expected subsection, got " ++ show v)

infixr 1 <|>
(<|>) :: ConfigParser a -> ConfigParser a -> ConfigParser a
(<|>) a b = ConfigParser $ \v ->
  case unConfigParser a v of
    Right r -> Right r
    Left _ -> unConfigParser b v

infix 0 <?>
(<?>) :: ConfigParser a -> String -> ConfigParser a
(<?>) a lbl = a <|> fail lbl

withDefault :: ConfigParser a -> a -> ConfigParser a
withDefault a d = a <|> return d

