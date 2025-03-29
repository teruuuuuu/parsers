{-# LANGUAGE OverloadedStrings #-}
module Parser.JsonSpec where
import Test.HUnit

import Parser.Json
import Parser.Common
import Parser.Json (JValue(JVArray, JVObject, JVBool), jvindex, jvprop)

jsonSpecTests :: Test
jsonSpecTests = TestList
  [ 
      "json parse success1" ~: parseJson "{}" ~?= PSuccess (JVObject []) ""
      , "json parse success2" ~: parseJson " { \"a\" : 1 } " ~?= PSuccess (JVObject [("a", JVNumber 1)]) ""
      , "json parse success3" ~: parseJson " { \"a\" : 1, \"b\": []} " ~?= PSuccess (JVObject [("a", JVNumber 1), ("b", JVArray [])]) ""
      , "json parse success4" ~: parseJson " { \"a\" : 1, \"b\": [{ \"c\": true}]} " ~?= PSuccess (JVObject [("a", JVNumber 1), ("b", JVArray [JVObject [("c", JVBool True)]])]) ""
      , "json parse success5" ~: case parseJson "{\"a\":1,\"b\":[{\"c\":true}]}" of 
            PSuccess jv n -> Just jv >>= jvprop "b" >>= jvindex 0 >>= jvprop "c" 
            _ -> Nothing
            ~?= Just (JVBool True)
      , "json parse nested" ~: parseJson "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": [1, {\"f\": \"g\"}]}}}}}" ~?= 
            PSuccess (JVObject [("a", JVObject [("b", JVObject [("c", JVObject [("d", JVObject [("e", JVArray [JVNumber 1, JVObject [("f", JVString "g")]])])])])])]) ""
      , "json parse deeply nested" ~: case parseJson "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": [1, {\"f\": \"g\"}]}}}}}" of
            PSuccess jv n -> Just jv >>= jvprop "a" >>= jvprop "b" >>= jvprop "c" >>= jvprop "d" >>= jvprop "e" >>= jvindex 1 >>= jvprop "f"
            _ -> Nothing
            ~?= Just (JVString "g")
  ]

