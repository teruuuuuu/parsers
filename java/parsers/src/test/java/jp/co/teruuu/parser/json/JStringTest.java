package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JStringResult;
import org.junit.Test;

public class JStringTest {

    @Test
    public void test() {
        JString parser = new JString();

        ParseResult result1 = parser.parse("\"abcdefg\"");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(new JStringResult("abcdefg")));

        ParseResult result2 = parser.parse("\"abc\\\"defg\"");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals(new JStringResult("abc\"defg")));
    }
}
