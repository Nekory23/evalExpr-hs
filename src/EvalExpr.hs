module EvalExpr where

import Parser

errorParser :: String
errorParser = "The expression is not valid"

-- conseils pour le bttf :

-- * faire le removeSpaces avec la foction filter
-- * trouver comment faire un vrai AST (arbre) :
--      exemple: 2+2*5          
--      2    +     x
--                 *
--              2      5
-- * implémenter les functors (évitera le gros bordel de caseof... en théorie)
--        * si j'y arrive pas : essayer de faire des fonctions bcp plus courtes et lisibles
-- éviter le pattern matching trop redondant et inutile pour certaines fonctions

-- dans les choses qui passaient déja pas :
-- * gerer mieux les parentheses
-- * gerer mieux les priorités de calculs (problemes avec les calculs du type : 5 - 100 + 5)
-- * gerer mieux les espaces et le fait qu'il faut les enlever petit a petit
-- * gerer mieux les divisions par 0


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

computeRes :: Float -> Float -> Char -> Maybe Float
computeRes nbr nbr2 '+' = Just (nbr + nbr2)
computeRes nbr nbr2 '-' = Just (nbr - nbr2)
computeRes nbr nbr2 '^' = Just (nbr ** nbr2)
computeRes nbr nbr2 '*' = Just (nbr * nbr2)
computeRes nbr 0 '/' = Nothing
computeRes nbr nbr2 '/' = Just (nbr / nbr2)
computeRes _ _ _ = Nothing

checkNextOp :: (Float, String) -> Either String (Float, String)
checkNextOp (res, []) = Right (res, [])
checkNextOp (res, [char]) = Left $ "checkNextOp: " ++ errorParser
checkNextOp (res, str) = case runParser (parseAnyChar "+-*/^") str of
        Just (sign, str') -> Right (res, str)
        Nothing -> Left $ "checkNextOp: " ++ errorParser

getBaseNbr :: Char -> Float
getBaseNbr '*' = 1
getBaseNbr '/' = 1
getBaseNbr '^' = 1
getBaseNbr _ = 0

manageMultiplePar :: (Float, String) -> Char -> Either String (Float, String)
manageMultiplePar (res, str) sign =
    case getResultParenthesis (getBaseNbr sign, str) sign of
    Right (nr, ns) -> case runParser (parseChar ')') ns of
        Just (_, ns') -> getResultAddSub (res, [sign] ++ show nr ++ ns')
        Nothing -> Left $ "par: " ++ errorParser
    Left error -> Left error

getResultParenthesis :: (Float, String) -> Char -> Either String (Float,String)
getResultParenthesis (res, str) sign = case runParser (parseChar '(') str of
    Just (_, rest) -> case runParser parseFloat rest of
        Just (nres, rest') -> case getResultAddSub (nres, rest') of
            Right (nres', rest'') -> case runParser (parseChar ')') rest'' of
                Just (_, rest''') -> case computeRes res nres' sign of
                    Just result -> getResultAddSub (result, rest''')
                    Nothing -> Left "Division by 0"
                Nothing -> Left $ "par: " ++ errorParser
            Left error -> Left error
        Nothing -> case manageMultiplePar (res, rest) sign of
            Right (nr, ns) -> Right (nr, ns)
            Left error -> Left error
    Nothing -> Left $ "par: " ++ errorParser

getResultPow :: (Float, String) -> Either String (Float, String)
getResultPow (res, []) = Right (res, [])
getResultPow (res, [')']) = Right (res, [')'])
getResultPow (res, [_]) = Left $ "pow: " ++ errorParser
getResultPow (res, str@(')':xs)) = Right (res, str)
getResultPow (res, str) = case runParser (parseAnyChar "^") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> case computeRes res nbr sign of
            Just result -> getResultAddSub (result, str'')
            Nothing -> Left "Division by 0"
        Nothing -> case getResultParenthesis (res, str') sign of 
            Right (nres, nstr) -> Right (nres, nstr)
            Left error -> Left error
    Nothing -> case checkNextOp (res, str) of
        Right (sign, str) -> Right (res, str)
        Left error -> Left error

getResultMulDiv :: (Float, String) -> Either String (Float, String)
getResultMulDiv (res, []) = Right (res, [])
getResultMulDiv (res, [')']) = Right (res, [')'])
getResultMulDiv (res, [_]) = Left $ "mul/div: " ++ errorParser
getResultMulDiv (res, str@(')':xs)) = Right (res, str)
getResultMulDiv (res, '/':'0':xs) = Left $ "div: " ++ errorParser
getResultMulDiv (res, str) = case runParser (parseAnyChar "*/") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> case getResultPow (nbr, str'') of
            Right (nr, ns) -> case computeRes res nr sign of 
                Just result -> getResultAddSub (result, ns)
                Nothing -> Left "Division by 0"
            Left error -> Left error
        Nothing -> case getResultParenthesis (res, str') sign of 
            Right (nres, nstr) -> Right (nres, nstr)
            Left error -> Left error
    Nothing -> case getResultPow (res, str) of
        Right (newres, newstr) -> Right (newres, newstr)
        Left error -> Left error

getResultAddSub :: (Float, String) -> Either String (Float, String)
getResultAddSub (res, []) = Right (res, [])
getResultAddSub (res, [')']) = Right (res, [')'])
getResultAddSub (res, [_]) = Left $ "add/sub: " ++ errorParser
getResultAddSub (res, str@(')':xs)) = Right (res, str)
getResultAddSub (res, str) = case runParser (parseAnyChar "+-") str of
    Just (sign, str') -> case runParser parseFloat str' of
        Just (nbr, str'') -> case getResultMulDiv (nbr, str'') of
            Right (nr, ns) -> case computeRes res nr sign of
                Just result -> getResultAddSub (result, ns)
                Nothing -> Left "Division by 0"
            Left error -> Left error
        Nothing -> case getResultParenthesis (res, str') sign of 
            Right (nres, nstr) -> Right (nres, nstr)
            Left error -> Left error
    Nothing -> case getResultMulDiv (res, str) of
        Right (newres, newstr) -> getResultAddSub (newres, newstr)
        Left error -> Left error

evaluateExpr :: String -> Either String Float
evaluateExpr expr = let clean = removeSpace expr in
    case runParser parseFloat clean of
    Just (nbr, rest) -> case getResultAddSub (nbr, rest) of
        Right (res, []) -> Right res
        Right (res, _) -> Left $ "evalexpr: " ++ errorParser
        Left error -> Left error
    Nothing -> case getResultAddSub (0, "+" ++ clean) of
        Right (res, []) -> Right res
        Right (res, _) -> Left $ "evalexpr: " ++ errorParser
        Left error -> Left error