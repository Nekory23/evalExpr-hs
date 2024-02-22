import System.Environment (getArgs)
import System.IO ( stderr, hPutStrLn )
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith, exitFailure)
import Text.Printf ( printf )

import Parser
import ErrorHandling ( checkArgs )
import EvalExpr

displayError :: String -> IO a
displayError error = 
    hPutStrLn stderr ("Error: " ++ error) >> exitWith (ExitFailure 84)

displayResult :: Float -> IO ()
displayResult = printf "%.2f\n"

main :: IO ()
main = do
    args <- getArgs
    case checkArgs args of
        Right args' -> case evaluateExpr $ head args of
            Right res -> displayResult res
            Left error -> displayError error
        Left error -> displayError error
