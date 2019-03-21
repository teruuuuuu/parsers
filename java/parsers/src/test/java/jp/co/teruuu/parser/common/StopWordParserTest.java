package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import org.junit.Test;

public class StopWordParserTest {

    @Test
    public void test() {
        Parser<String> parser = Parser.stop("World");

        ParseResult result1 = parser.parse("Hello, World");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals("Hello, "));

        ParseResult result2 = parser.parse("World");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals(""));
    }
}
