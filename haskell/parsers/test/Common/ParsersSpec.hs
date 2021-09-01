module Common.ParsersSpec where
import Test.HUnit

import Common.Parser

parserTest :: Test
parserTest = TestList 
  [
      "string parser test 1" ~: parse (StringParser "abc") "abcdef" ~?= PSuccess {result = PString "abc", next = "def"}
      , "string parser test 2" ~: parse (StringParser "abc") "bcdef" ~?= PFail {message = "not match abc", next = "bcdef"}
      , "or parser test 1" ~: parse (OrParser [StringParser "abc", StringParser "def"] ) "abc" ~?= PSuccess {result = PString "abc", next = ""}
      , "or parser test 2" ~: parse (OrParser [StringParser "abc", StringParser "def"] ) "defghi" ~?= PSuccess {result = PString "def", next = "ghi"}
      , "or parser test 3" ~: parse (OrParser [StringParser "abc", StringParser "def"] ) "bcd" ~?= PFail {message = "OrParser Not Match", next = "bcd"}
      , "and parser test 1" ~: parse (AndParser [StringParser "abc", StringParser "def"] ) "abcdefghi" ~?= PSuccess {result = PList [PString "abc",PString "def"], next = "ghi"}
      , "and parser test 2" ~: parse (AndParser [StringParser "abc", StringParser "def"] ) "abcabc" ~?= PFail {message = "AndParser Not Match", next = "abcabc"}
  ]

