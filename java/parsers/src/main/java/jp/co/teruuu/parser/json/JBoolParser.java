package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.JBoolean;
import jp.co.teruuu.parser.json.type.JValue;

public class JBoolParser implements JParser<JValue> {
  Parser<String> parser;
  public JBoolParser() {
    this.parser = Parser.string("true").or(Parser.string("false"));
  }

  @Override
  public ParseResult<JValue> parse(String input) {
    String next = input;
    ParseResult result = this.parser.parse(next);
    if(result instanceof ParseResult.Success) {
      String value = ((ParseResult.Success<String>) result).value;
      next = ((ParseResult.Success<String>) result).next;
      if(value.equals("true")) {
        new JBoolean(true);
        return new ParseResult.Success(new JBoolean(true), next);
      } else {
        return new ParseResult.Success(new JBoolean(false), next);
      }

    }
    return result;
  }
}
