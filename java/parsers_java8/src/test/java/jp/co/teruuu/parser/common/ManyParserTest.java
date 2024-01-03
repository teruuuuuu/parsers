package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

public class ManyParserTest {

    @Test
    public void test() {
        Parser<List<String>> manyParser = Parser.string("Hello").many();

        ParseResult result1 = manyParser.parse("HelloHelloHello");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(Arrays.asList("Hello", "Hello", "Hello")));

        ParseResult result2 = manyParser.parse("Hello HelloHello");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals(Arrays.asList("Hello")));

        ParseResult result3 = manyParser.parse("helloHelloHello");
        assert(result3 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result3).value.equals(Arrays.asList()));
    }
}
