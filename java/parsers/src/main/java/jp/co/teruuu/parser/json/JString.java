package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Pair3;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;
import jp.co.teruuu.parser.json.type.JStringResult;

import java.util.Arrays;

public class JString implements JParser<JStringResult> {
    Parser<Tuple3<String, String, String>> parser;
    public JString() {
        this.parser = new Pair3(Parser.string("\""),
                Parser.stopWithEscape("\"", Arrays.asList(new Tuple2<>("\\\"", "\""))),
                Parser.string("\""));

    }

    @Override
    public ParseResult<JStringResult> parse(String input) {
        ParseResult result = this.parser.parse(input);
        if(result instanceof ParseResult.Success) {
            JStringResult value = new JStringResult(((ParseResult.Success<Tuple3<String, String, String>>) result).value.item2);
            String next = ((ParseResult.Success<Tuple3<String, String, String>>) result).next;
            return new ParseResult.Success<>(value, next);
        }
        return result;
    }
}
