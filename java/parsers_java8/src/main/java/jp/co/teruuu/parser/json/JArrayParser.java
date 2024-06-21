package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Pair2;
import jp.co.teruuu.parser.common.Pair3;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;
import jp.co.teruuu.parser.json.type.JArray;
import jp.co.teruuu.parser.json.type.JString;
import jp.co.teruuu.parser.json.type.JValue;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class JArrayParser implements JParser<JValue> {
    Parser<List<String>> sp = Parser.string(" ").many();
    Parser<Tuple3<List<String>, String, List<String>>> left = new Pair3(sp, Parser.string("["), sp);
    Parser<Tuple3<List<String>, String, List<String>>> right = new Pair3(sp, Parser.string("]"), sp);

    public JArrayParser() {
    }

    @Override
    public ParseResult<JValue> parse(String input) {
        String next = input;
        ParseResult leftResult = this.left.parse(next);
        if(leftResult instanceof ParseResult.Success) {
            next = ((ParseResult.Success) leftResult).next;
            Parser<Optional<Tuple2<Tuple2<JValue, List<String>>, List<Tuple2<String, Tuple3<List<String>, JValue, List<String>>>>>>> parser =
                    (new Pair2(new Pair2(JParser.value(), sp), (new Pair2(Parser.string(","), new Pair3(sp, JParser.value(), sp))).many())).option();
            ParseResult result = parser.parse(next);
            if(result instanceof ParseResult.Success) {
                next = ((ParseResult.Success) result).next;
                ParseResult rightResult = this.right.parse(next);

                if(rightResult instanceof ParseResult.Success) {
                    Optional<Tuple2<Tuple2<JValue, List<String>>, List<Tuple2<String, Tuple3<List<String>, JValue, List<String>>>>>> resultValue =
                            ((ParseResult.Success<Optional<Tuple2<Tuple2<JValue, List<String>>, List<Tuple2<String, Tuple3<List<String>, JValue, List<String>>>>>>>) result).value;
                    next = ((ParseResult.Success) rightResult).next;
                    JArray ret = new JArray();
                    if(resultValue.isPresent()) {
                        ret.add(resultValue.get().item1.item1);
                        for(Tuple2<String, Tuple3<List<String>, JValue, List<String>>> a: resultValue.get().item2) {
                            ret.add(a.item2.item2);
                        }
                    }
                    return new ParseResult.Success(ret, next);
                } else {
                    return rightResult;
                }
            } else {
                return result;
            }
        } else {
            return leftResult;
        }
    }
}
