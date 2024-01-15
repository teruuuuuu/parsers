package jp.co.teruuu.parser.json;

import jp.co.teruuu.common.Tuple;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;

import java.util.stream.Collectors;

// https://www.json.org/json-en.html
public class JsonParser implements Parser<Json> {

    @Override
    public ParseResult<Json> parse(String input, int location) {
        return jsonParser.parse(input);
    }

    private Parser<Json> jsonParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            return jStringParer.or(jNumberParser).or(jBooleanParser).or(jNullParser).or(jArrayParser).or(jObjectParser).withSkipSpace();
        }
    };


    private Parser<Json> jStringParer = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            return Parser.dquoteString().map(JString::new);
        }
    };

    private Parser<Json> jNumberParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            return Parser.number().map(JNumber::new);
        }
    };

    private Parser<Json> jBooleanParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<Json> trueParser = Parser.skip("true").map(v -> new JBoolean(true));
            Parser<Json> falseParser = Parser.skip("false").map(v -> new JBoolean(true));
            return trueParser.or(falseParser);
        }
    };

    private Parser<Json> jNullParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            return Parser.skip("null").map(v -> new JNull());
        }
    };

    private Parser<Json> jArrayParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            return jsonParser.array(Parser.skip('[').withSkipSpace(), Parser.skip(']').withSkipSpace(),
                    Parser.skip(',').withSkipSpace()).map(JArray::new);
        }
    };

    private Parser<Json> jObjectParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<Tuple<String,Json>> entryParer = Parser.dquoteString().withSkipSpace().andLeft(Parser.skip(':').withSkipSpace()).and(jsonParser);
            return entryParer.array(Parser.skip('{').withSkipSpace(), Parser.skip('}').withSkipSpace(), Parser.skip(",").withSkipSpace()).map(value ->
                    new JObject(value.stream().collect(Collectors.toMap(Tuple::fst, Tuple::snd))));
        }
    };

    abstract class JsonParserBase<T> implements Parser<T> {
        Parser<T> parser;
        protected abstract Parser<T> genParser();

        @Override
        public ParseResult<T> parse(String input, int location) {
            if (parser == null) {
                this.parser = genParser();
            }
            return parser.parse(input, location);
        }
    }
}
