package jp.co.teruuu.parser.common;


import org.junit.Test;

public class StringParserTest {

    @Test
    public void test() {
        Parser<String> strParser1 = Parser.string("Hello");
        ParseResult<String> result1 = strParser1.parse("HelloWorld");
        assert(result1 instanceof ParseResult.Success);

        ParseResult<String> result2 = strParser1.parse("Hello, World");
        assert(result2 instanceof ParseResult.Success);

        ParseResult<String> result3 = strParser1.parse("hello, World");
        assert(result3 instanceof ParseResult.Failure);

    }
}
