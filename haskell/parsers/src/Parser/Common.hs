module Parser.Common  where

import Control.Applicative ( Alternative(empty, (<|>)) ) 

data ParseResult a  = PSuccess {result::a, next::String} | PFail {message::String, next::String} deriving (Eq, Show)
newtype Parser a = Parser (String -> ParseResult a)

parse :: Parser a -> String -> ParseResult a
parse (Parser p) inp = p inp

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = Parser (\inp -> case parse p inp of
            PSuccess r n -> PSuccess (g r) n
            PFail m n -> PFail m inp
        )
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = Parser (\inp -> PSuccess v inp)
    
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Parser (\inp -> case parse pg inp of
            PSuccess r n -> parse (fmap r px) n
            PFail m n -> PFail m inp
        )

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\inp -> case parse p inp of
            PSuccess r n -> parse (f r) n
            PFail m n -> PFail m inp
        )

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\inp -> PFail "" inp)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser (\inp -> case parse p inp of
            PSuccess r m -> PSuccess r m 
            PFail m n -> parse q inp
        )

pchar :: Char -> Parser Char
pchar c = Parser (\inp -> case inp of
    (h:tail) | c == h -> PSuccess c tail
    _ -> PFail ("not " ++ [c]) inp)

pany :: Parser Char
pany = Parser (\inp -> case inp of
        (h:tail) -> PSuccess h tail
        _ -> PFail "empty" inp
    )

pstr :: String -> Parser String
pstr s = Parser (\inp -> case inp of
        x | s == take (length s) inp -> PSuccess s $ drop (length s) inp
        _ -> PFail ("not " ++ s) inp
    )

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
repeat1 p = Parser (\inp -> case repeat' p inp [] of
        PSuccess r n | length r > 0 -> PSuccess r n
        _ -> PFail "" inp
    )

repeatBy :: Parser a -> Parser b -> Parser [a]
repeatBy p sep = do
    h <- p
    t <- repeat0 $ snd <$> (sep `pand` p)
    pure $ h : t

repeat' :: Parser a -> String -> [a] -> ParseResult [a]
repeat' p inp c = case parse p inp of
    PSuccess r n -> repeat' p n (r:c) 
    PFail r n -> PSuccess (reverse c) n 

pnot :: Parser a -> Parser Char
pnot p = Parser (\inp -> case parse p inp of
        PFail m n | length n > 0 -> PSuccess (head inp) (tail inp)
        _ -> PFail "" inp
    )

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
escape = fmap (\(a,b) -> case b of 
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