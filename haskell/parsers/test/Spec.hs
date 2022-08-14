{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.HUnit

import Parser.JsonSpec (jsonSpecTests)

main :: IO ()
main = do
  runTestTT $ TestList
    [ 
        jsonSpecTests
    ]
  return ()