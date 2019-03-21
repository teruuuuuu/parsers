package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.JNull;
import jp.co.teruuu.parser.json.type.JValue;

public class JNullParser implements JParser<JValue> {
  Parser<String> parser;
  public JNullParser() {
    this.parser = Parser.string("null");
  }

  @Override
  public ParseResult<JValue> parse(String input) {
    String next = input;
    ParseResult result = this.parser.parse(next);
    if(result instanceof ParseResult.Success) {
      next = ((ParseResult.Success<String>) result).next;
      return new ParseResult.Success(new JNull(), next);
    }
    return result;
  }
}
