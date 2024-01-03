package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import org.junit.Test;

public class OrParserTest {

    @Test
    public void test() {
        Parser<String> orParser = Parser.string("Hello").or(Parser.string("World"));
        ParseResult result1 = orParser.parse("Hello World");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals("Hello"));

        ParseResult result2 = orParser.parse("World Hello");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals("World"));

        ParseResult result3 = orParser.parse("world Hello");
        assert(result3 instanceof ParseResult.Failure);
    }
}
