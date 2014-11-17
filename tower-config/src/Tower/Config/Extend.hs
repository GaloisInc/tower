{-# LANGUAGE OverloadedStrings #-}

module Tower.Config.Extend where

import           Control.Applicative
import           Data.Either (lefts, rights)
import qualified Data.Map as M
import           Data.Scientific hiding (scientific)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

import Text.TOML (process)
import Text.TOML.Parser

extendConfig :: TOML -> [String] -> TOML
extendConfig root@(TOML rmap) opts = case parseOpts opts of
  Right t -> TOML (M.insertWith aux "args" (Left t) rmap)
  Left _ -> root
  where
  aux (Left (TOML new)) (Left (TOML old)) = Left (TOML (M.union new old))
  aux new _ = new

parseOpts :: [String] -> Either String TOML
parseOpts as = case lefts kvs of
  [] -> Right (process (rights kvs))
  es -> Left (unlines ("Errors parsing options:":es))
  where
  kvs = map argKeyVal as


argKeyVal:: String -> Either String Token
argKeyVal s = case s of
  ('-':'-':ss) -> parseTok (B.pack ss)
  _ -> parseTok (B.pack s)

parseTok :: B.ByteString -> Either String Token
parseTok b = eitherResult (feed (fmap Right (parse parseKV b)) B.empty)

parseKV :: Parser (B.ByteString, TOMLV)
parseKV = do
  k <- keyv
  v <- equal *> parseVal
  return (k,v)
  where
  keyv = lexeme (takeWhile1 (notInClass " \t\n="))
  equal = lexeme (string "=")
  spc = char ' ' <|> char '\t'
  lexeme p = do { x <- p; _ <- many spc; return x }

parseVal :: Parser TOMLV
parseVal = (array <?> "array")
       <|> (bool  <?> "bool")
       <|> (str   <?> "string")
       <|> (num   <?> "number")
  where
  array = VArray <$> between lbrace rbrace (value `sepBy` comma)
  bool = VBool <$> (true *> return True <|> false *> return False)
  str = VString <$> many (notChar ' ')
  num = do
    n <- lexeme $ scientific
    case floatingOrInteger n of
      Right i -> return (VInteger i)
      Left f -> return (VDouble f)

  true = lexeme (string "true")
  false = lexeme (string "false")
  lbrace = lexeme (string "[")
  rbrace = lexeme (string "]")
  comma  = lexeme (string ",")
  spc = char ' ' <|> char '\t'
  lexeme p = do { x <- p; _ <- many spc; return x }

  between a b p = do { _ <- a ; e <- p; _ <- b; return e }

