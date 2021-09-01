module Common.Parser where

import Data.List

class Parser a where
    parse :: a -> String -> ParseResult

data PValue = PString String | PList [PValue] |PEmpty deriving (Eq,Show)

data ParseResult  = PSuccess {result::PValue, next::String} | PFail {message::String, next::String} deriving (Eq, Show)
isPSuccess :: ParseResult -> Bool
isPSuccess (PSuccess a b) = True
isPSuccess _ = False

data CommonParsers = StringParser String
  | OrParser [CommonParsers] 
  | RepeatParser CommonParsers
  | AndParser [CommonParsers] deriving Show

instance Parser CommonParsers where
    parse (StringParser a) x = if isPrefixOf a x then PSuccess (PString a) $ drop (length a) x else PFail ("not match " ++ a) x
    parse (OrParser parsers) x = (\a -> case a of 
            Just x -> x 
            Nothing -> PFail "OrParser Not Match"  x
        ) $ find isPSuccess $ map (\p -> parse p x)  parsers
    parse (RepeatParser parser) x = case parse parser x of
        PSuccess a b -> case parse (RepeatParser parser) b of
            PSuccess (PList c) d -> PSuccess (PList $ a:c) d
        PFail _ b -> PSuccess (PList []) b
    parse (AndParser parsers) x = foldl (\acc curp -> case acc of
            PSuccess (PList a) b -> case parse curp b of
                PSuccess (PList c) d -> PSuccess (PList $ a ++ c) d
                PSuccess c d -> PSuccess (PList $ a ++ [c]) d
                PFail _ d -> PFail "AndParser Not Match" x
            PFail _ b -> PFail "AndParser Not Match" x
        ) (PSuccess (PList []) x) parsers
