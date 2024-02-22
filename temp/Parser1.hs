module Parser1 where
import Data.Maybe

type Parser a = String -> Maybe (a, String)

-- parse a specific char
parseChar :: Char -> Parser Char
parseChar c [] = Nothing
parseChar c (x:xs)
    | c == x = Just (c, xs)
    | otherwise = Nothing

-- parse any char from the given str
parseAnyChar :: String -> Parser Char
parseAnyChar [] [] = Nothing
parseAnyChar [] _ = Nothing
parseAnyChar _ [] = Nothing
parseAnyChar (x:xs) list@(y:ys)
    | isNothing $ parseChar x list = parseAnyChar xs list
    | otherwise = Just (x, ys)

-- applies the first or the second parse
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str = case p1 str of
    Nothing -> p2 str
    Just res -> Just res

-- applies both the first and second parse
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str = case p1 str of
    Nothing -> Nothing
    Just res -> case p2 $ snd res of
        Nothing -> Nothing
        Just res2 -> Just ((fst res, fst res2), snd res2)

-- applies both the first and second parse and applies a function to the result
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 str = case p1 str of
    Nothing -> Nothing
    Just (parsed, rest) -> case p2 rest of
        Nothing -> Nothing
        Just (parsed2, rest2) -> Just (func parsed parsed2, rest2)

-- tries to apply it until it fails (parseMany itself can never fail)
parseMany :: Parser a -> Parser [a]
parseMany p str = case p str of
    Just (parsed, rest) -> case parseMany p rest of
        Just (parsed', rest') -> Just (parsed:parsed', rest')
        Nothing -> Just ([parsed], rest)
    Nothing -> Just ([], str)

-- parseMany but must parse at least 1 elem
parseSome :: Parser a -> Parser [a]
parseSome p str = case p str of
    Just (parsed, rest) -> case parseMany p rest of
        Just (parsed', rest') -> Just (parsed:parsed', rest')
        Nothing -> Just ([parsed], rest)
    Nothing -> Nothing

-- parse an unsigned int
parseUInt :: Parser Int
parseUInt [] = Nothing
parseUInt str = case parseSome (parseAnyChar ['0'..'9']) str of
    Just (parsed, rest) -> Just (read parsed::Int, rest)
    Nothing -> Nothing
--parseUInt str = readMaybe $ parseMany (parseAnyChar ['0'..'9']) str

-- parse a signed int
parseInt :: Parser Int
parseInt [] = Nothing
parseInt ('-':rest) = case parseUInt rest of
    Just (nbr, rest) -> Just (nbr * (-1), rest)
    Nothing -> Nothing
parseInt str = parseUInt str

-- parse a tuple
parseTuple :: Parser a -> Parser (a,a)
parseTuple p str = case parseChar '(' str of
    Just (_, str') -> case p str' of
        Just (nbr, str'') -> case parseChar ',' str'' of
            Just (_, str''') -> case p str''' of
                Just (nbr2, rest) -> case parseChar ')' rest of
                    Just (_, rest') -> Just ((nbr, nbr2), rest')
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
--parseTuple str = func ( *> | func read <*> | func , *> | func read <* | func )