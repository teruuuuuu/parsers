module Parser.Common  where

import Control.Applicative ( Alternative(empty, (<|>)) ) 

data ParseResult a = PSuccess { result :: a, next :: String }
                   | PFail { message :: String, next :: String }
                   deriving (Eq, Show)

newtype Parser a = Parser {runParser :: String -> ParseResult a }

parse :: Parser a -> String -> ParseResult a
parse = runParser

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = Parser $ \inp -> case parse p inp of
        PSuccess r n -> PSuccess (g r) n
        PFail m n    -> PFail m n

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = Parser $ PSuccess v
    
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Parser $ \inp -> case parse pg inp of
        PSuccess r n -> parse (fmap r px) n
        PFail m n    -> PFail m n


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser $ \inp -> case parse p inp of
        PSuccess r n -> parse (f r) n
        PFail m n    -> PFail m n

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ \inp -> PFail "" inp

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser $ \inp -> case parse p inp of
            PSuccess r m -> PSuccess r m 
            PFail _ _ -> parse q inp

pchar :: Char -> Parser Char
pchar c = Parser $ \inp -> case inp of
    (h:rest) | c == h -> PSuccess c rest
    _ -> PFail ("not " ++ [c]) inp

pany :: Parser Char
pany = Parser $ \inp -> case inp of
        (h:rest) -> PSuccess h rest
        _ -> PFail "empty" inp

pstr :: String -> Parser String
pstr s = Parser $ \inp -> case inp of
        _ | s == take (length s) inp -> PSuccess s $ drop (length s) inp
        _ -> PFail ("not " ++ s) inp

pnumchar :: Parser Char
pnumchar = pchar '0' <|> pchar '1' <|> pchar '2' <|> pchar '3' <|> pchar '4' <|> pchar '5' <|> pchar '6' <|> pchar '7' <|> pchar '8' <|> pchar '9'

pnumber :: Parser Int
pnumber = read <$> repeat1 pnumchar

pbool :: Parser Bool
pbool = (True <$ pstr "true") <|> (False <$ pstr "false")

pspace :: Parser Char
pspace = pchar ' ' <|> pchar '\t' <|> pchar '\b' <|> pchar '\f' <|> pchar '\r' <|> pchar '\n'

skipSpace :: Parser String
skipSpace = chararray2str <$> repeat0 pspace

repeat0 :: Parser a -> Parser [a]
repeat0 p = Parser (\inp -> repeat' p inp [] )

repeat1 :: Parser a -> Parser [a]
repeat1 p = Parser $ \inp -> case repeat' p inp [] of
        PSuccess r n | not (null r) -> PSuccess r n
        _ -> PFail "" inp

repeat1By :: Parser a -> Parser b -> Parser [a]
repeat1By p sep = do
    h <- p
    t <- repeat0 $ snd <$> (sep `pand` p)
    pure $ h : t

repeat0By :: Parser a -> Parser b -> Parser [a]
repeat0By p sep = Parser $ \inp -> case parse (repeat1By p sep) inp of
        PSuccess r n -> PSuccess r n
        _ -> PSuccess [] inp


repeat' :: Parser a -> String -> [a] -> ParseResult [a]
repeat' p inp c = case parse p inp of
    PSuccess r n -> repeat' p n (r:c) 
    PFail _ n -> PSuccess (reverse c) n 

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

pnot :: Parser a -> Parser Char
pnot p = Parser $ \inp -> case parse p inp of
        PFail _ _ -> case uncons inp of
            Just (h, t) -> PSuccess h t
            Nothing -> PFail "empty input" inp
        _ -> PFail "" inp

pand :: Parser a -> Parser b -> Parser (a,b)
pand pa pb = do
    ra <- pa
    rb <- pb
    pure (ra, rb)

pstop :: Char -> Parser String
pstop c = fmap chararray2str $ repeat0 $ pnot $ pchar c

chararray2str :: [Char] -> String
chararray2str c = c

char2str :: Char -> String
char2str c = [c]

escape :: Parser Char
escape = fmap (\(_,b) -> case b of 
        't' -> '\t'
        'f' -> '\f'
        'b' -> '\b'
        'r' -> '\r'
        'n' -> '\n'
        '\\' -> '\\'
        '"' -> '\"'
        '\'' -> '\''
        _ -> b
    ) $ pchar '\\' `pand` pany 

stopWithEscape :: Char ->  Parser String
stopWithEscape c = chararray2str <$> repeat0 ( escape <|> pnot (pchar c))

showParse :: Show a => Parser a -> IO ()
showParse p = getLine >>= pure . parse p >>= print

-- showParse :: Show a => Parser a -> IO ()
-- showParse p = do
--     inp <- getLine
--     r <- pure $ parse p inp
--     print r
