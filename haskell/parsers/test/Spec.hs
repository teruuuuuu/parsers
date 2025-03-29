{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.HUnit

import Parser.CommonSpec (testPchar, testPany, testPstr, testSkipSpace, testRepeat0, testRepeat1, testPand, testPnot)
import Parser.JsonSpec (jsonSpecTests)

main :: IO ()
main = do
  runTestTT $ TestList
    [
        testPchar, testPany, testPstr, testSkipSpace, testRepeat0, testRepeat1, testPand, testPnot,
        jsonSpecTests
    ]
  return ()