module Parser.Json  where

import Control.Applicative ( Alternative(empty, (<|>)) ) 
import qualified Data.Map as Map 
import Data.List (find)
import Parser.Common 

data JValue = JVObject [(String, JValue)] | JVArray [JValue] |JVString String | JVNumber Int | JVBool Bool | JVNull deriving (Eq, Show)

jvprop :: String -> JValue -> Maybe JValue
jvprop key jv = case jv of
    JVObject l -> snd <$> find (\ a -> fst a == key) l
    _ -> Nothing

jvindex :: Int -> JValue -> Maybe JValue
jvindex index jv = case jv of
    JVArray l | length l > index -> Just $ l!!index
    _ -> Nothing

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

pjarray :: Parser JValue
pjarray = do
    _ <- skipSpace
    _ <- pchar '['
    _ <- skipSpace
    r <- JVArray <$> repeat0By pjvalue (skipSpace `pand` pchar ',' `pand` skipSpace)
    _ <- skipSpace
    _ <- pchar ']'
    pure r 

pjobject :: Parser JValue
pjobject = do
    _ <- skipSpace
    _ <- pchar '{'
    _ <- skipSpace
    r <- JVObject <$> repeat0By pjobjectItem (skipSpace `pand` pchar ',' `pand` skipSpace)
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
