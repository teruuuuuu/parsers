package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Pair2;
import jp.co.teruuu.parser.common.Pair3;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;
import jp.co.teruuu.parser.json.type.JValue;

import java.util.List;

public class JsonParser implements JParser<JValue> {
  Parser<List<String>> sp = Parser.string(" ").many();

  @Override
  public ParseResult<JValue> parse(String input) {
    Parser<Tuple2<Tuple3<List<String>, JValue, List<String>>, String>> parser = new Pair2(new Pair3(sp, JParser.object().or(JParser.array()), sp), Parser.EOF());
    ParseResult result = parser.parse(input);
    if(result instanceof ParseResult.Success) {
      Tuple2<Tuple3<List<String>, JValue, List<String>>, String> resultV = ((ParseResult.Success<Tuple2<Tuple3<List<String>, JValue, List<String>>, String>>) result).value;
      return new ParseResult.Success<>(resultV.item1.item2, ((ParseResult.Success<Tuple2<Tuple3<List<String>, JValue, List<String>>, String>>) result).next);
    } else {
      return result;
    }
  }
}
