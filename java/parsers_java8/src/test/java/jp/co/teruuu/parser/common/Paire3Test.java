package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple3;
import org.junit.Test;

public class Paire3Test {

    @Test
    public void test() {
        Parser<Tuple3<String, String, String>> parser = Parser.string("Hello").pair3(Parser.string(":"), Parser.string("World"));

        ParseResult result1 = parser.parse("Hello:World");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(new Tuple3<String, String, String>("Hello", ":", "World")));

        ParseResult result2 = parser.parse("Hello : World");
        assert(result2 instanceof ParseResult.Failure);
    }
}
