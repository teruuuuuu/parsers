import Test.HUnit
import Common.ParsersSpec


main :: IO ()
main = do
    putStrLn "test start"
    runTestTT $ TestList [ parserTest]
    putStrLn "test end"

