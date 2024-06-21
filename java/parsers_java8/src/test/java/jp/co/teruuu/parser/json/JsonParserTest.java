package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import org.junit.Test;

public class JsonParserTest {

  @Test
  public void test() {
    JParser parser = JParser.json();

    ParseResult result1 = parser.parse("[1,2,3,4]");
    assert(result1 instanceof ParseResult.Success);

    ParseResult result2 = parser.parse(" {\"a\": [ [1, 2, 3,4], {} ]} ");
    assert(result2 instanceof ParseResult.Success);
  }
}
