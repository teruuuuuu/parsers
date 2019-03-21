package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JString;
import org.junit.Test;

public class JStringParserTest {

    @Test
    public void test() {
        JStringParser parser = new JStringParser();

        ParseResult result1 = parser.parse("\"abcdefg\"");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(new JString("abcdefg")));

        ParseResult result2 = parser.parse("\"abc\\\"defg\"");
        assert(result2 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result2).value.equals(new JString("abc\"defg")));
    }
}
