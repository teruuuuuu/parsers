package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class OptionalParserTest {

    @Test
    public void test() {
        Parser<Optional<List<String>>> parser = Parser.string("Hello").plus().option();

        ParseResult result1 = parser.parse("HelloHelloHello");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success<Optional<List<String>>>) result1).value.isPresent());
        assert(((ParseResult.Success<Optional<List<String>>>) result1).value.get().equals(Arrays.asList("Hello", "Hello", "Hello")));

        ParseResult result2 = parser.parse("Hello HelloHello");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success<Optional<List<String>>>) result2).value.isPresent());
        assert(((ParseResult.Success<Optional<List<String>>>) result2).value.get().equals(Arrays.asList("Hello")));


        ParseResult result3 = parser.parse("helloHelloHello");
        assert(result3 instanceof ParseResult.Success);
        assert(!((ParseResult.Success<Optional<List<String>>>) result3).value.isPresent());
    }
}
