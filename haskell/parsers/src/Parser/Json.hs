module Parser.Json  where

import Control.Applicative ( Alternative((<|>)) ) 
-- import qualified Data.Map as Map 
import Data.List (find)
import Parser.Common 

data JValue 
    = JVObject [(String, JValue)] 
    | JVArray [JValue] 
    | JVString String 
    | JVNumber Int 
    | JVBool Bool 
    | JVNull 
    deriving (Eq, Show)

jvprop :: String -> JValue -> Maybe JValue
jvprop key (JVObject jvo) = snd <$> find (\(k, _) -> k == key) jvo
jvprop _ _ = Nothing

jvindex :: Int -> JValue -> Maybe JValue
jvindex index (JVArray jva) | length jva > index = Just $ jva !! index
jvindex _ _ = Nothing

parseJson :: String -> ParseResult JValue
parseJson = parse pjson

pjson :: Parser JValue
pjson = pjobject <|> pjarray

pjvalue :: Parser JValue
pjvalue = pjobject <|> pjarray <|> pjstr <|> pjnum <|> pjbool

pjstr :: Parser JValue
pjstr = do
    _ <- pchar '"'
    s <- stopWithEscape '"' 
    _ <- pchar '"'
    pure $ JVString s

pjnum :: Parser JValue
pjnum = JVNumber <$> pnumber

pjbool :: Parser JValue
pjbool = (JVBool True <$ pstr "true") <|> (JVBool False <$ pstr "false")

pjnull :: Parser JValue
pjnull = JVNull <$ pstr "null"

commaSep :: Parser ()
commaSep = skipSpace *> pchar ',' *> skipSpace *> pure ()

pjarray :: Parser JValue
pjarray = do
    _ <- skipSpace
    _ <- pchar '['
    _ <- skipSpace
    r <- JVArray <$> repeat0By pjvalue commaSep
    _ <- skipSpace
    _ <- pchar ']'
    pure r 

pjobject :: Parser JValue
pjobject = do
    _ <- skipSpace
    _ <- pchar '{'
    _ <- skipSpace
    r <- JVObject <$> repeat0By pjobjectItem commaSep
    _ <- skipSpace
    _ <- pchar '}'
    _ <- skipSpace
    pure r

pjobjectItem :: Parser (String, JValue)
pjobjectItem = do
    _ <- pchar '"'
    k <- stopWithEscape '"' 
    _ <- pchar '"'
    _ <- skipSpace
    _ <- pchar ':'
    _ <- skipSpace
    v <- pjvalue
    _ <- skipSpace
    pure (k,v)

