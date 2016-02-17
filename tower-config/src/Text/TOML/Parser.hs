{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Text.TOML.Parser
  ( module Text.TOML.Value
  , document
  , keygroup
  , keyval
  , value
  , Token
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Scientific (floatingOrInteger)

import Text.TOML.Value


type Token = Either [B.ByteString] (B.ByteString, TOMLV)

document :: Parser [Token]
document = smb *> many ekk <* endOfInput
  where
    smb = skipMany blank
    ekk = (eitherP keygroup keyval) <* smb

keygroup :: Parser [B.ByteString]
keygroup = do
    skipMany blank
    between lbrace rbrace skey
  where skey = keyg `sepBy` period

keyval :: Parser (B.ByteString, TOMLV)
keyval = do
    k <- keyv
    v <- equal *> value
    return (k, v)

keyg :: Parser B.ByteString
keyg = lexeme $ takeWhile1 $ notInClass " \t\n]."
keyv :: Parser B.ByteString
keyv = lexeme $ takeWhile1 $ notInClass " \t\n="

value :: Parser TOMLV
value = (array <?> "array")
    <|> (bool  <?> "bool")
    <|> (str   <?> "string")
    <|> (num   <?> "number")
  where
    array = VArray <$> between lbrace rbrace (value `sepBy` comma)
    bool = VBool <$> (true *> return True <|> false *> return False)
    str = VString <$> between quote quote (many (notChar '"'))
    num = do
        n <- lexeme $ scientific
        case floatingOrInteger n of
            Right i -> return $ VInteger i
            Left d -> return $ VDouble d

whatever :: Parser a -> Parser ()
whatever p = p >> return ()
lexeme :: Parser a -> Parser a
lexeme p = do { x <- p; _ <- many spc; return x }
spc :: Parser Char
spc   = char ' ' <|> char '\t'
comment :: Parser ()
comment = whatever $ char '#' *> takeTill (=='\n')
line :: Parser a -> Parser ()
line p = p *> (lexeme endOfLine)
blank :: Parser ()
blank = line $ lexeme $ (try comment) <|> return ()

quote :: Parser B.ByteString
quote = lexeme $ string "\""
lbrace :: Parser B.ByteString
lbrace = lexeme $ string "["
rbrace :: Parser B.ByteString
rbrace = lexeme $ string "]"
comma :: Parser B.ByteString
comma = lexeme $ string ","
period :: Parser B.ByteString
period = lexeme $ string "."
equal :: Parser B.ByteString
equal = lexeme $ string "="
true :: Parser B.ByteString
true = lexeme $ string "true"
false :: Parser B.ByteString
false = lexeme $ string "false"

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b p = do { _ <- a; e <- p; _ <- b; return e }
