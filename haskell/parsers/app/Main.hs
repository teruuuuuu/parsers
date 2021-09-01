module Main where

import Lib
import Common.Parser

main :: IO ()
main = do
    print $ parse (StringParser "abc") "abcdef"
