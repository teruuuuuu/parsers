package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import org.junit.Test;

public class JObjectParserTest {

  @Test
  public void test() {
    JParser parser = JParser.object();
    ParseResult result1 = parser.parse("{}");
    assert(result1 instanceof ParseResult.Success);

    ParseResult result2 = parser.parse("{\"a\": 1}");
    assert(result2 instanceof ParseResult.Success);

    ParseResult result3 = parser.parse("{\"a\": 1, \"c\": []}");
    assert(result3 instanceof ParseResult.Success);

    ParseResult result4 = parser.parse("{\"a\": 1, \"c\": [{\"d\": 345}]}");
    assert(result4 instanceof ParseResult.Success);
  }
}
