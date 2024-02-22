module ErrorHandling where

checkArgs :: [String] -> Either String [String]
checkArgs [] = Left "This program must take an expression as argument"
checkArgs args
    | length args == 1 = Right args
    | otherwise = Left "This program must only take 1 arg"