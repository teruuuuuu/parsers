package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import org.junit.Test;

public class Paire2Test {

    @Test
    public void test() {
        Parser<Tuple2<String, String>> parser = Parser.string("Hello").pair2(Parser.string("World"));

        ParseResult result1 = parser.parse("HelloWorld");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(new Tuple2<String, String>("Hello", "World")));

        ParseResult result2 = parser.parse("Hello World");
        assert(result2 instanceof ParseResult.Failure);
    }
}
