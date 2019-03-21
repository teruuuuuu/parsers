package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.json.type.JArray;
import jp.co.teruuu.parser.json.type.JBoolean;
import jp.co.teruuu.parser.json.type.JNull;
import jp.co.teruuu.parser.json.type.JNumber;
import jp.co.teruuu.parser.json.type.JString;
import org.junit.Test;

import java.util.Arrays;

public class JArrayParserTest {

    @Test
    public void test() {
        JParser parser = JParser.array();

        ParseResult result1 = parser.parse("[]");
        assert(result1 instanceof ParseResult.Success);

        ParseResult result2 = parser.parse("[ 1, \"2\", true, null]");
        assert(result2 instanceof ParseResult.Success);
        assert((ParseResult.Success) result2).value.equals(new JArray(
                Arrays.asList(
                        new JNumber(1, 0, 0),
                        new JString("2"),
                        new JBoolean(true),
                        new JNull()
                )));
    }
}
