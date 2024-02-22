import Test.HUnit

import Parser
import EvalExpr
import Control.Exception.Base (evaluate)


parserTests :: Test
parserTests =
    TestList [
        "parseChar" ~: Just ('a', "bcd") ~=? runParser (parseChar 'a') "abcd",
        "parseChar" ~: Nothing ~=? runParser (parseChar 'a') "zbcd",
        "parseChar" ~: Nothing ~=? runParser (parseChar 'b') "abcd",
        "parseChar" ~: Just ('a', "aaa") ~=? runParser (parseChar 'a') "aaaa",
        "parseAnyChar" ~: Just ('a', "bcd") ~=? runParser (parseAnyChar "a") "abcd",
        "parseAnyChar" ~: Nothing ~=? runParser (parseAnyChar "zxy") "abcd",
        "parseAnyChar" ~: Just ('c', "def") ~=? runParser (parseAnyChar "bca") "cdef",
        "parseOr" ~: Just('a', "bcd") ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd",
        "parseOr" ~: Just('b', "cda") ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda",
        "parseOr" ~: Nothing ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz",
        "parseAnd" ~: Just(('a','b'), "cd") ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd",
        "parseAnd" ~: Nothing ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda",
        "parseAnd" ~: Nothing ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd",
        "parseAndWith" ~: Just ("ab", "cd") ~=? runParser (parseAndWith (\ x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd",
        "parseMany" ~: Just("   ", "foobar") ~=? runParser (parseMany (parseChar ' ')) "   foobar",
        "parseMany" ~: Just("", "foobar   ") ~=? runParser (parseMany (parseChar ' ')) "foobar   ",
        "parseSome" ~: Just("42", "foobar") ~=? runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar",
        "parseSome" ~: Nothing ~=? runParser (parseSome(parseAnyChar ['0'..'9'])) "foobar42",
        "parseUInt" ~: Just(42, "foobar") ~=? runParser parseUInt "42foobar",
        "parseUInt" ~: Nothing  ~=? runParser parseUInt "foobar34",
        "parseInt" ~: Just(42, "foobar") ~=? runParser parseInt "42foobar",
        "parseInt" ~: Just(-42, "foobar") ~=? runParser parseInt "-42foobar",
        "parseInt" ~: Nothing  ~=? runParser parseInt "foobar34",
        "parsetuple" ~: Just((123,456), "foo bar") ~=? runParser (parseTuple parseInt) "(123,456)foo bar",
        "parsetuple" ~: Nothing ~=? runParser (parseTuple parseInt) "(123456)foo bar",
        "parseFloat" ~: Just(42.0, "foobar") ~=? runParser parseFloat "42foobar",
        "parseFloat" ~: Just(-42.0, "foobar") ~=? runParser parseFloat "-42foobar",
        "parseFloat" ~: Just(42.3, "foobar") ~=? runParser parseFloat "42.3foobar",
        "parseFloat" ~: Just(-42.3, "foobar") ~=? runParser parseFloat "-42.3foobar",
        "parseFloat" ~: Nothing  ~=? runParser parseFloat "foobar-6",
        "parseUfloat" ~: Just(42.0, "foobar") ~=? runParser parseUFloat "42foobar",
        "parseUfloat" ~: Just(42.69, "foobar") ~=? runParser parseUFloat "42.69foobar",
        "parseUfloat" ~: Nothing  ~=? runParser parseUFloat  "foobar34"
    ]

simpleComputeTests :: Test
simpleComputeTests =
    TestList [
        "comptuRes" ~: Just 3.0 ~=? computeRes 1 2 '+',
        "comptuRes" ~: Just 30.33 ~=? computeRes 30 0.33 '+',
        "comptuRes" ~: Just (-3.0) ~=? computeRes (-1) (-2) '+',
        "comptuRes" ~: Just (-1.0) ~=? computeRes 1 2 '-',
        "comptuRes" ~: Just 100.0 ~=? computeRes 200 100 '-',
        "comptuRes" ~: Just 4.0 ~=? computeRes 2 2 '^',
        "comptuRes" ~: Just 1.0 ~=? computeRes 1 2 '^',
        "comptuRes" ~: Just 1.0 ~=? computeRes (-1) 2 '^',
        "comptuRes" ~: Just 3.0 ~=? computeRes 1.5 2 '*',
        "comptuRes" ~: Just 2.0 ~=? computeRes 1 2 '*',
        "comptuRes" ~: Just 300.0 ~=? computeRes 150 2 '*',
        "comptuRes" ~: Just 200.0 ~=? computeRes 1 200 '*',
        "comptuRes" ~: Just 0.5 ~=? computeRes 1 2 '/',
        "comptuRes" ~: Nothing ~=? computeRes 1 0 '/',
        "comptuRes" ~: Just 0 ~=? computeRes 0 1 '/',
        "comptuRes" ~: Just (-0.5) ~=? computeRes (-1) 2 '/',
        "comptuRes" ~: Just 50 ~=? computeRes 100 2 '/'
    ]

removeSpaceTests :: Test
removeSpaceTests =
    TestList [
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\n\ncoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\n\n",
        "removeSpace" ~: "coucou" ~=? removeSpace "\n\ncoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\n\n",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \n   coucou   \n  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\t\tcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\t\t",
        "removeSpace" ~: "coucou" ~=? removeSpace "\t\tcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\t\t",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \t   coucou   \t  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\v\vcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\v\v",
        "removeSpace" ~: "coucou" ~=? removeSpace "\v\vcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\v\v",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \v   coucou   \v  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\b\bcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\b\b",
        "removeSpace" ~: "coucou" ~=? removeSpace "\b\bcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\b\b",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \b   coucou   \b  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\a\acoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\a\a",
        "removeSpace" ~: "coucou" ~=? removeSpace "\a\acoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\a\a",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \a   coucou   \a  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\r\rcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\r\r",
        "removeSpace" ~: "coucou" ~=? removeSpace "\r\rcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\r\r",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \r   coucou   \r  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\f\fcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\f\f",
        "removeSpace" ~: "coucou" ~=? removeSpace "\f\fcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\f\f",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \f   coucou   \f  ",
        "removeSpace" ~: "coucou" ~=? removeSpace "\n\t\v\b\a\r\fcoucou",
        "removeSpace" ~: "coucou" ~=? removeSpace "coucou\n\t\v\b\a\r\f",
        "removeSpace" ~: "coucou" ~=? removeSpace "\n\t\v\b\a\r\fcoucou     ",
        "removeSpace" ~: "coucou" ~=? removeSpace "     coucou\n\t\v\b\a\r\f",
        "removeSpace" ~: "coucou" ~=? removeSpace "  \n\t\v\b\a\r\f   coucou   \n\t\v\b\a\r\f  "
    ]

getBaseNbrTests :: Test
getBaseNbrTests =
    TestList [
        "getBaseNbr" ~: 1.0 ~=? getBaseNbr '*',
        "getBaseNbr" ~: 1.0 ~=? getBaseNbr '^',
        "getBaseNbr" ~: 1.0 ~=? getBaseNbr '/',
        "getBaseNbr" ~: 0.0 ~=? getBaseNbr '+',
        "getBaseNbr" ~: 0.0 ~=? getBaseNbr '-'
    ]

evaluateExprTests :: Test
evaluateExprTests =
    TestList [
        "evaluateExpr" ~: Right 3.0 ~=? evaluateExpr "2+1",
        "evaluateExpr" ~: Right 5.0 ~=? evaluateExpr "2^2+1",
        "evaluateExpr" ~: Right 33.0 ~=? evaluateExpr "(5+6)*3",
        "evaluateExpr" ~: Right 0.5 ~=? evaluateExpr "1/2",
        "evaluateExpr" ~: Right 39.0 ~=? evaluateExpr "(6+7)*3",
        "evaluateExpr" ~: Right (-3.0) ~=? evaluateExpr "5-8",
        "evaluateExpr" ~: Right 5.0 ~=? evaluateExpr "(((5)))",
        "evaluateExpr" ~: Right 200.0 ~=? evaluateExpr "(5+5)*2*(5+5)",
        "evaluateExpr" ~: Right 50.0 ~=? evaluateExpr "((2*5)*2)+(3*(5+5))",
        "evaluateExpr" ~: Left "Division by 0" ~=? evaluateExpr "2/(0^2)",
        "evaluateExpr" ~: Left "Division by 0" ~=? evaluateExpr "2/(1-1)"
    ]

main :: IO Counts
main =
    runTestTT parserTests >>
    runTestTT simpleComputeTests >>
    runTestTT removeSpaceTests >>
    runTestTT getBaseNbrTests >>
    runTestTT evaluateExprTests
