package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JNumber;
import org.junit.Test;

public class JNumberParserTest {

    @Test
    public void test() {
        JParser parser = new JNumberParser();

        ParseResult result1 = parser.parse("+123.456E-123");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success) result1).value.equals(new JNumber(123,456, -123)));
    }
}
