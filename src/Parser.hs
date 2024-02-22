module Parser where

import Data.Maybe
--import Control.Monad (Functor)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

--instance Functor Parser where
--    fmap fct parser = undefined 

-- parse a specific char
parseChar :: Char -> Parser Char
parseChar char = Parser f where
    f [] = Nothing
    f (x:xs)
        | x == char = Just (x, xs)
        | otherwise = Nothing

-- parse any char from the given str
-- use elem
parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser (f str) where
    f [] [] = Nothing
    f [] _ = Nothing
    f _ [] = Nothing
    f (x:xs) list@(y:ys)
        | isNothing $ runParser (parseChar x) list = f xs list
        | otherwise = Just (x, ys)

-- applies both the first or second parse
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser f where
    f str = case runParser p1 str of
        Nothing -> runParser p2 str
        Just res -> Just res

-- applies both the first and second parse
parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd p1 p2 = Parser f where
    f str = case runParser p1 str of
        Just (parsed, rest) -> case runParser p2 rest of
            Just (parsed', rest') -> Just ((parsed, parsed'), rest')
            Nothing -> Nothing
        Nothing -> Nothing

-- applies both the first and second parse and applies a function to the result
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser f where
    f str = case runParser p1 str of
        Just (parsed, rest) -> case runParser p2 rest of
            Just (parsed', rest') -> Just (func parsed parsed', rest')
            Nothing -> Nothing
        Nothing -> Nothing

-- tries to apply it until it fails (parseMany itself can never fail)
parseMany :: Parser a -> Parser [a]
parseMany p = Parser (f p) where
    f p str = case runParser p str of
        Just (parsed, rest) -> case f p rest of
            Just (parsed', rest') -> Just (parsed:parsed', rest')
            Nothing -> Just ([parsed], rest)
        Nothing -> Just ([], str)

-- parseMany but must parse at least 1 elem
parseSome :: Parser a -> Parser [a]
parseSome p = Parser (f p) where
    f p str = case runParser p str of
        Just (parsed, rest) -> case f p rest of
            Just (parsed', rest') -> Just (parsed:parsed', rest')
            Nothing -> Just ([parsed], rest)
        Nothing -> Nothing

-- parse an unsigned int
parseUInt :: Parser Int
parseUInt = Parser f where
    f str = case runParser (parseSome (parseAnyChar ['0'..'9'])) str of
        Just (parsed, rest) -> Just (read parsed::Int, rest)
        Nothing -> Nothing

-- parse a signed int
parseInt :: Parser Int
parseInt = Parser f where
    f ('-':rest) = case runParser parseUInt rest of
        Just (nbr, rest') -> Just (nbr * (-1), rest')
        Nothing -> Nothing
    f str = runParser parseUInt str

-- parse an unsigned float
parseUFloat :: Parser Float
parseUFloat = Parser f where
    f str = case runParser (parseSome (parseAnyChar ['0'..'9'])) str of
        Just (parsed, rest) -> case runParser (parseChar '.') rest of
            Just (p, rest') -> 
                case runParser (parseSome (parseAnyChar ['0'..'9'])) rest' of
                    Just (parsed', rest'') -> 
                        let nbr = parsed ++ [p] ++ parsed' in
                        Just (read nbr::Float, rest'')
                    Nothing -> Nothing
            Nothing -> Just (read parsed::Float, rest)
        Nothing -> Nothing

-- parse a float
parseFloat :: Parser Float
parseFloat = Parser f where
    f ('-':rest) = case runParser parseUFloat rest of
        Just (nbr, rest') -> Just (nbr * (-1), rest')
        Nothing -> Nothing
    f str = runParser parseUFloat str

-- parse a tuple
parseTuple :: Parser a -> Parser (a,a)
parseTuple p = Parser f where
    f str = case runParser (parseChar '(') str of
        Just (_, str') -> case runParser p str' of
            Just (nbr, str'') -> case runParser (parseChar ',') str'' of
                Just (_, str''') -> case runParser p str''' of
                    Just (nbr2, rest) -> case runParser (parseChar ')') rest of
                        Just (_, rest') -> Just ((nbr, nbr2), rest')
                        Nothing -> Nothing
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing