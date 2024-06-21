package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple2;
import org.junit.Test;

import java.util.Arrays;

public class StopWithEscapeTest {

    @Test
    public void test() {
        Parser<String> parser = Parser.stopWithEscape("\"", Arrays.asList(new Tuple2<>("\\\"", "\"")));

        ParseResult result1 = parser.parse("Hello\", World");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals("Hello"));

        ParseResult result2 = parser.parse("Hello\\\", World");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals("Hello\", World"));
    }
}
