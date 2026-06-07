module Parser where

import Control.Applicative (Alternative(empty), (<|>))

type Parse a = String -> Maybe (String, a)
newtype Parser a = Parser (String -> Maybe (String, a))

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Nothing -> Nothing
        Just (rest, x) -> Just (rest, f x)

instance Applicative Parser where
    pure p = Parser (\input -> Just (input, p))

    liftA2 f (Parser a) (Parser b) = Parser $ \input -> case a input of
        Nothing -> Nothing
        Just (input', a') -> case b input' of 
            Nothing -> Nothing
            Just (input'', b') -> Just (input'', f a' b')

instance Alternative Parser where
    empty = Parser $ const Nothing

    (<|>) (Parser p1) (Parser p2) =
        Parser $ \s ->
            case p1 s of
                Nothing -> p2 s
                result  -> result

parseChar :: Char -> Parse Char
parseChar _ [] = Nothing
parseChar ch (x:xs)
    | ch == x = Just (xs, ch)
    | otherwise = Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
        f [] = Nothing
        f (x:xs)
            | p x       = Just (xs, x)
            | otherwise = Nothing
