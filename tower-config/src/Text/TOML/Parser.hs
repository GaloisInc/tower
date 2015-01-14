{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Text.TOML.Parser
  ( module Text.TOML.Value
  , document
  , keygroup
  , keyval
  , value
  , Token
  )where

import Control.Applicative

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

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

keyg = lexeme $ takeWhile1 $ notInClass " \t\n]."
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
        n <- lexeme $ number
        case n of
            I n -> return $ VInteger n
            D d -> return $ VDouble d

whatever p = p >> return ()
lexeme p = do { x <- p; many spc; return x }
spc   = char ' ' <|> char '\t'
comment = whatever $ char '#' *> takeTill (=='\n')
line p = p *> (lexeme endOfLine)
blank = line $ lexeme $ (try comment) <|> return ()

zee = lexeme $ string "Z"
quote = lexeme $ string "\""
lbrace = lexeme $ string "["
rbrace = lexeme $ string "]"
comma = lexeme $ string ","
period = lexeme $ string "."
equal = lexeme $ string "="
true = lexeme $ string "true"
false = lexeme $ string "false"

between a b p = do { a; e <- p; b; return e }
