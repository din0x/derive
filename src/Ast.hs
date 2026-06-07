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
binary op p = ((flip Infix <$> p) <*> whitespace opParser <*> whitespace self) <|> p
  where
    self = binary op p
    opParser = op <$ char (opToChar op)

whitespace :: Parser a -> Parser a
whitespace p = (many (satisfy isSpace)) *> p <* (many (satisfy isSpace))

simple :: Parser Expr
simple = Number <$> int <|> Symbol <$> symbol <|> parenthesized parser

char :: Char -> Parser Char
char ch = Parser $ parseChar ch

int :: Parser Int
int = read <$> some (satisfy isDigit)

symbol :: Parser String
symbol = some $ satisfy isLetter

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

parenthesized :: Parser a -> Parser a
parenthesized p = (\_ a _ -> a) <$> char '(' <*> p <*> char ')'
