package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.json.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ParserTest {

    @Test
    public void test() {
        ParseResult<String> parseResult1 = Parser.dquoteString().parse("\"abc\\\"def\"");
        assertTrue(parseResult1 instanceof ParseResult.Success<String>);
        ParseResult.Success<String> parseSuccess1 = (ParseResult.Success<String>) parseResult1;
        assertEquals(parseSuccess1.value(), "abc\"def");

        Parser<List<String>> arrayParser = Parser.dquoteString().array(Parser.charP('['), Parser.charP(']'), Parser.charP(','));
        arrayParser.parse("[ \"abc\" ,\"def\"]");

        JsonParser.jValueParser.parse("[1,2,3,true, false]");


        JsonParser.parse("{ \"aaa\" : []}");
    }
}
