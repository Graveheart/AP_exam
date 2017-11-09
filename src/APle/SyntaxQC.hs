import AST
import Syntax

import Test.HUnit
import Test.QuickCheck

-- Test strings
tiny :: String
tiny = "0 + t = t.\n" ++
        "t + 0 = t.\n" ++
        "t1 + (t2 + t3) = (t1 + t2) + t3.\n" ++
        "t + t = 2 * t.\n" ++
        "0 * t = 0."

rules1 :: String
rules1 = "t ** n = t * t ** (n + ~1) | num(n)."

rules2 :: String
rules2 = "D(x,y) = 0 | var(y), lexless(x,y)."

rules3 :: String
rules3= "n1 * n2 = n3 | num(n1), num(n2), mul(n1,n2;n3)."

rules4 :: String
rules4 = "n1 * n2 = n3 | num(;n1+n2)."

-- Tests
invalidParseTests :: IO()
invalidParseTests = do
    testInvalidParse "t =123"
    -- testInvalidParse "t =-123123123"
    -- testInvalidParse "t = 123_13"
    -- testInvalidParse "t ='12"
    -- testInvalidParse "_Aas43das%dt ='13'"
    -- testInvalidParse "t ='13"
    -- testInvalidParse "undefined = 1"
    -- testInvalidParse "for = 1"
    -- testInvalidParse "[for i of []]"
    -- testInvalidParse "[for (i of [-1])"
    -- testInvalidParse "[for () i]"
    -- testInvalidParse "[for (i of [3]) if(p == 2) ]"
    -- testInvalidParse "[if (1+1) 1 ]"
    -- testInvalidParse "[]=[]"
    -- testInvalidParse "t =-"
    -- testInvalidParse "d = --"
    -- testInvalidParse "d = 'asdsadad\asd'"

testInvalidParse :: String -> IO()
testInvalidParse input =
    case parseStringCmds input of
        Left _ ->  print "Test passed"
        Right _ ->
            error ("Test failed: "
                ++ show input
                ++ " is a valid parse")

testTiny, testRules1, testRules2 :: Test
testTiny = TestCase (assertEqual "ok: " (
    Right [
            CRule (Rule (TFun "+" [TNum 0,TVar "t"]) (TVar "t") []),
            CRule (Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") []),
            CRule (Rule (TFun "+" [TVar "t1",TFun "+" [TVar "t2",TVar "t3"]])
                        (TFun "+" [TFun "+" [TVar "t1",TVar "t2"],TVar "t3"]) []),
            CRule (Rule (TFun "+" [TVar "t",TVar "t"]) (TFun "*" [TNum 2,TVar "t"]) []),
            CRule (Rule (TFun "*" [TNum 0,TVar "t"]) (TNum 0) [])]
    ) (parseStringCmds tiny))

testRules1 = TestCase (assertEqual "ok: " (
    Right
        [CRule
            (Rule
                (TFun "**" [TVar "t",TVar "n"])
                (TFun "*"
                    [TVar "t",
                     TFun "**" [TVar "t",
                                TFun "+" [TVar "n",TNum (-1)]]])
                [Cond "num" [TVar "n"] []])]
    ) (parseStringCmds rules1))

testRules2 = TestCase (assertEqual "ok: " (
    Right
        [CRule
            (Rule
                (TFun "D" [TVar "x",TVar "y"])
                (TNum 0)
                [Cond "var" [TVar "y"] [],
                 Cond "lexless" [TVar "x",TVar "y"] []])]
    ) (parseStringCmds rules2))

tests :: Test
tests = TestList [
    TestLabel "Parsing tiny: " testTiny,
    TestLabel "Parsing rules 1: " testRules1,
    TestLabel "Parsing rules 2: " testRules2
    ]

-- main test function, to make tests runnable as "runhaskell SyntaxQC"
main :: IO Counts
main = do
        runTestTT tests
        -- invalidParseTests
