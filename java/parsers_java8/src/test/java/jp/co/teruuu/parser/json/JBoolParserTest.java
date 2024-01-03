package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JBoolean;
import jp.co.teruuu.parser.json.type.JString;
import org.junit.Test;

public class JBoolParserTest {

    @Test
    public void test() {
        JParser parser = new JBoolParser();

        ParseResult result1 = parser.parse("true");
        assert(result1 instanceof ParseResult.Success);
        assert((ParseResult.Success) result1).value.equals(new JBoolean(true));

        ParseResult result2 = parser.parse("false");
        assert(result2 instanceof ParseResult.Success);
        assert((ParseResult.Success) result2).value.equals(new JBoolean(false));

        ParseResult result3 = parser.parse("Talse");
        assert(result3 instanceof ParseResult.Failure);
    }
}
