module Parser.CommonSpec where

import Test.HUnit
import Parser.Common

-- Test cases for Parser.Common
testPchar :: Test
testPchar = TestList
    [ "pchar success" ~: parse (pchar 'a') "abc" ~?= PSuccess 'a' "bc"
    , "pchar failure" ~: parse (pchar 'a') "bcd" ~?= PFail "not a" "bcd"
    ]

testPany :: Test
testPany = TestList
    [ "pany success" ~: parse pany "abc" ~?= PSuccess 'a' "bc"
    , "pany failure" ~: parse pany "" ~?= PFail "empty" ""
    ]

testPstr :: Test
testPstr = TestList
    [ "pstr success" ~: parse (pstr "hello") "hello world" ~?= PSuccess "hello" " world"
    , "pstr failure" ~: parse (pstr "hello") "hi world" ~?= PFail "not hello" "hi world"
    ]

testSkipSpace :: Test
testSkipSpace = TestList
    [ "skipSpace success" ~: parse skipSpace "   abc" ~?= PSuccess "   " "abc"
    , "skipSpace no spaces" ~: parse skipSpace "abc" ~?= PSuccess "" "abc"
    ]

testRepeat0 :: Test
testRepeat0 = TestList
    [ "repeat0 success" ~: parse (repeat0 (pchar 'a')) "aaabc" ~?= PSuccess "aaa" "bc"
    , "repeat0 no matches" ~: parse (repeat0 (pchar 'a')) "bc" ~?= PSuccess "" "bc"
    ]

testRepeat1 :: Test
testRepeat1 = TestList
    [ "repeat1 success" ~: parse (repeat1 (pchar 'a')) "aaabc" ~?= PSuccess "aaa" "bc"
    , "repeat1 failure" ~: parse (repeat1 (pchar 'a')) "bc" ~?= PFail "" "bc"
    ]

testPand :: Test
testPand = TestList
    [ "pand success" ~: parse (pchar 'a' `pand` pchar 'b') "abc" ~?= PSuccess ('a', 'b') "c"
    , "pand failure" ~: parse (pchar 'a' `pand` pchar 'b') "ac" ~?= PFail "not b" "c"
    ]

testPnot :: Test
testPnot = TestList
    [ "pnot success" ~: parse (pnot (pchar 'a')) "bc" ~?= PSuccess 'b' "c"
    , "pnot failure" ~: parse (pnot (pchar 'a')) "abc" ~?= PFail "" "abc"
    ]