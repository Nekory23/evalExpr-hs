module EvalExpr where

import GHC.Float (powerDouble)
import Data.Char (isSpace)

import Parser

errorParser :: String
errorParser = "The expression is not valid"

-- OK i can do infinite + and - or * and / --

--getResultMulDiv :: (Float, String) -> Either String (Float, String)
--getResultMulDiv (res, str) = case runParser (parseAnyChar "*/") str of
--    Just (sign, str') -> case runParser parseFloat str' of 
--        Just (nbr, str'') -> getResultMulDiv (computeRes res nbr sign, str'')
--        Nothing -> Left $ "mul: " ++ errorParser
--    Nothing -> Right (res, str)

--getResultAddSub :: (Float, String) -> Either String Float
--getResultAddSub (res, str) = case runParser (parseAnyChar "+-") str of
--    Just (sign, str') -> case runParser parseFloat str' of
--        Just (nbr, str'') -> getResultAddSub (computeRes res nbr sign, str'')
--        Nothing -> Left errorParser -- call "parseMul" on renverra une erreur que depuis parseExposant / si on trouve pas de parenthese qui ferme
--    Nothing -> Right res

-- final --

computeRes :: Float -> Float -> Char -> Float
computeRes nbr nbr2 '+' = nbr + nbr2
computeRes nbr nbr2 '-' = nbr - nbr2
computeRes nbr nbr2 '*' = nbr * nbr2
computeRes nbr nbr2 '/' = nbr / nbr2
computeRes nbr nbr2 '^' = nbr ** nbr2
computeRes _ _ _ = 0

removeSpace :: String -> String
removeSpace (' ':xs) = removeSpace xs
removeSpace ('\n':xs) = removeSpace xs
removeSpace ('\t':xs) = removeSpace xs
removeSpace ('\v':xs) = removeSpace xs
removeSpace ('\b':xs) = removeSpace xs
removeSpace ('\a':xs) = removeSpace xs
removeSpace ('\r':xs) = removeSpace xs
removeSpace ('\f':xs) = removeSpace xs
removeSpace (c:cs) = c : removeSpace cs
removeSpace [] = []

checkNextOp :: (Float, String) -> Either String (Float, String)
checkNextOp (res, []) = Right (res, [])
checkNextOp (res, [char]) = Left $ "checkNextOp: " ++ errorParser
checkNextOp (res, str) = case runParser (parseAnyChar "+-*/^()") str of
        Just (sign, str') -> Right (res, str)
        Nothing -> Left $ "checkNextOp: " ++ errorParser

--getResultParenthesis :: (Float, String) -> Either String (Float, String)
--getResultParenthesis (_, [_]) = Left $ "parenthesis:" ++ errorParser
--getResultParenthesis (res, str) = case runParser (parseAnyChar "()") str of
--    Just ('(', str') -> case runParser parseFloat str' of
--        Just (nbr, str'') -> case getResultAddSub (nbr, str'') of
--            Right nr -> getResultParenthesis (nr, str'')
--            Left error -> Left error
--        Nothing -> Left $ "parenthesis:" ++ errorParser
--    Just (')', str') -> Right (res, str')
--    Nothing -> case checkNextOp (res, str) of
--        Right (_, str) -> Right (res, str)
--        Left error -> Left error
--
--getResultPow :: (Float, String) -> Either String (Float, String)
--getResultPow (_, [_]) = Left $ "exp: " ++ errorParser
--getResultPow (res, str) = case runParser (parseChar '^') str of
--    Just (sign, str') -> case runParser parseFloat str' of
--        Just (nbr, str'') -> case getResultParenthesis (nbr, str'') of
--            Right (nr, ns) -> getResultPow (computeRes res nr sign, ns)
--            Left error -> Left error
--        Nothing -> case getResultParenthesis (res, str') of
--            Right (nr, ns) -> getResultPow (computeRes res nr sign, ns)
--            Left error -> Left error
--    Nothing -> case getResultParenthesis (res, str) of
--        Right res -> Right res
--        Left error -> Left error

getResultPow :: (Float, String) -> Either String (Float, String)
getResultPow (res, [')']) = Right (res, [')'])
getResultPow (_, [_]) = Left $ "exp: " ++ errorParser
getResultPow (res, str@(')':xs)) = Right (res, str)
getResultPow (res, str) = case runParser (parseAnyChar "^") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> getResultPow (computeRes res nbr sign, str'')
        Nothing -> Left $ "exp: " ++ errorParser
    Nothing -> case checkNextOp (res, str) of
        Right (sign, str) -> Right (res, str)
        Left error -> Left error

getResultMulDiv :: (Float, String) -> Either String (Float, String)
getResultMulDiv (res, [')']) = Right (res, [')'])
getResultMulDiv (_, [_]) = Left $ "mul/div: " ++ errorParser
getResultMulDiv (res, str@(')':xs)) = Right (res, str)
getResultMulDiv (res, str) = case runParser (parseAnyChar "*/") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> case getResultPow (nbr, str'') of
            Right (nr, ns) -> getResultMulDiv (computeRes res nr sign, ns)
            Left error -> Left error
        Nothing -> case getResultPow (res, str') of
            Right (nr, ns) -> getResultMulDiv (computeRes res nr sign, ns)
            Left error -> Left error
    Nothing -> case getResultPow (res, str) of
        Right (newres, newstr) -> Right (newres, newstr)
        Left error -> Left error

getResultAddSub :: (Float, String) -> Either String (Float, String)
getResultAddSub (res, [')']) = Right (res, [')'])
getResultAddSub (res, []) = Right (res, [])
getResultAddSub (_, [_]) = Left $ "add/sub: " ++ errorParser
getResultAddSub (res, str@(')':xs)) = Right (res, str)
getResultAddSub (res, str) = case runParser (parseAnyChar "+-") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> case getResultMulDiv (nbr, str'') of
            Right (nr, ns) -> getResultAddSub (computeRes res nr sign, ns)
            Left error -> Left error
        Nothing -> case getResultMulDiv (res, str') of
            Right (nr, ns) -> getResultAddSub (computeRes res nr sign, ns)
            Left error -> Left error
    Nothing -> case getResultMulDiv (res, str) of
        Right (newres, newstr) -> getResultAddSub (newres, newstr)
        Left error -> Left error

evaluateExpr :: String -> Either String Float
evaluateExpr expr = let clean = removeSpace expr in
    case runParser parseFloat clean of
    Just (nbr, rest) -> case getResultAddSub (nbr, rest) of
        Right res -> Right $ fst res
        Left error -> Left error
    Nothing -> case runParser (parseChar '(') clean of
        Just (_, rest) -> case runParser parseFloat rest of
            Just (nbr, rest') -> case getResultAddSub (nbr, rest') of
                Right (res, str) -> case runParser (parseChar ')') str of 
                    Just (_, str') -> case getResultAddSub (res, str') of
                        Right res -> Right $ fst res
                        Left error -> Left error
                    Nothing -> Left $ "evalexpr: " ++ errorParser
                Left error -> Left error
            Nothing -> Left $ "evalexpr: " ++ errorParser
        Nothing -> Left $ "evalexpr: " ++ errorParser