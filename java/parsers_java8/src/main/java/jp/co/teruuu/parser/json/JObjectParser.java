package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Pair2;
import jp.co.teruuu.parser.common.Pair3;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;
import jp.co.teruuu.parser.json.type.JObject;
import jp.co.teruuu.parser.json.type.JString;
import jp.co.teruuu.parser.json.type.JValue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class JObjectParser implements JParser<JValue> {
  Parser<List<String>> sp = Parser.string(" ").many();
  Parser<Tuple3<List<String>, String, List<String>>> leftP = new Pair3(sp, Parser.string("{"), sp);
  Parser<Tuple3<List<String>, String, List<String>>> rightP = new Pair3(sp, Parser.string("}"), sp);
  Parser<Tuple3<List<String>, JString, List<String>>> keyP = new Pair3(sp, JParser.string(), sp);
  Parser<String> colonP = Parser.string(":");


  public JObjectParser(){}

  @Override
  public ParseResult<JValue> parse(String input) {
    String next = input;
    ParseResult leftResult = this.leftP.parse(next);
    if(leftResult instanceof ParseResult.Success) {
      next = ((ParseResult.Success) leftResult).next;
      Parser<Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>> kvP =
              new Pair3(keyP, colonP, new Pair3(sp, JParser.value(), sp));
      ParseResult kvResult = kvP.option().parse(next);
      if(kvResult instanceof ParseResult.Success) {
        Optional<Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>> kvValue =
                ((ParseResult.Success<Optional<Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>>>) kvResult).value;
        next = ((ParseResult.Success<Optional<Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>>>) kvResult).next;

        Map<JString, JValue> ret = new HashMap<JString, JValue>();
        if(kvValue.isPresent()) {
          ret.put(kvValue.get().item1.item2, kvValue.get().item3.item2);
          Parser kvsP = (new Pair2(Parser.string(","), kvP)).many();
          ParseResult kvsResult = kvsP.parse(next);

          if(kvsResult instanceof ParseResult.Success) {
            List<Tuple2<String, Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>>> kvsValue =
                    ((ParseResult.Success<List<Tuple2<String, Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>>>>) kvsResult).value;
            next = ((ParseResult.Success<List<Tuple2<String, Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>>>>) kvsResult).next;
            for(Tuple2<String, Tuple3<Tuple3<List<String>, JString, List<String>>, String, Tuple3<List<String>, JValue, List<String>>>> kvVal : kvsValue) {
              ret.put(kvVal.item2.item1.item2, kvVal.item2.item3.item2);
            }
          } else {
            return kvResult;
          }
        }

        ParseResult rightResult = this.rightP.parse(next);
        if(rightResult instanceof ParseResult.Success) {
          next = ((ParseResult.Success) rightResult).next;
          return new ParseResult.Success(new JObject(ret), next);
        } else {
          return rightResult;
        }
      } else {
        return kvResult;
      }
    } else {
      return leftResult;
    }
  }
}
