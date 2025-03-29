module Main where

import Parser.Common
import Parser.Json

parser :: Parser String
parser = do
    a <- repeat1 $ pchar 'a'
    b <- repeat1 $ pchar 'b'
    pure (a ++ b)

main :: IO ()
main = getLine >>= pure . parse pjson >>= print
    -- getLine Data.Functor.<&> parse parser
    -- where
    --     parser = repeat1 $ pchar 'a'
