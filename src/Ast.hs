module Ast where

import Control.Applicative (Alternative (some), many, (<|>))
import Data.Char (isDigit, isLetter, isSpace)
import Parser (Parser (Parser), parseChar, satisfy)

data Expr
    = Number Int
    | Symbol String
    | Infix Op Expr Expr
    deriving (Show, Eq)

data Op
    = Add
    | Mul
    deriving (Show, Eq)

opToChar :: Op -> Char
opToChar Add = '+'
opToChar Mul = '*'

parser :: Parser Expr
parser = whitespace additive

additive :: Parser Expr
additive = binary Add multiplicative

multiplicative :: Parser Expr
multiplicative = binary Mul simple

binary :: Op -> Parser Expr -> Parser Expr
binary op p = ((flip Infix <$> p) <*> whitespace opParser <*> whitespace p) <|> p
  where
    opParser = op <$ char (opToChar op)

whitespace :: Parser a -> Parser a
whitespace p = (many (satisfy isSpace)) *> p

simple :: Parser Expr
simple = Number <$> int <|> Symbol <$> symbol

char :: Char -> Parser Char
char ch = Parser $ parseChar ch

int :: Parser Int
int = read <$> some (satisfy isDigit)

symbol :: Parser String
symbol = some $ satisfy isLetter

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing
