package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JBoolean;
import jp.co.teruuu.parser.json.type.JNull;
import org.junit.Test;

public class JNullParserTest {

    @Test
    public void test() {
        JParser parser = new JNullParser();

        ParseResult result1 = parser.parse("null");
        assert(result1 instanceof ParseResult.Success);
        assert((ParseResult.Success) result1).value.equals(new JNull());

        ParseResult result2 = parser.parse("NULL");
        assert(result2 instanceof ParseResult.Failure);
    }
}
