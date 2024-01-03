package jp.co.teruuu.parser.json;

import jp.co.teruuu.common.Tuple;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;

import java.util.List;
import java.util.stream.Collectors;

public interface JsonParser {

    static ParseResult<Json> parse(String input) {
        return jsonParser.parse(input);
    }

    Parser<Json> jsonParser = new Parser<>() {

        Parser<JValue> parser;
        @Override
        public ParseResult<Json> parse(String input, int location) {
            if (parser == null) {
                parser = jObjectParser.or(jArrayParser);
            }
            switch (parser.parse(input, location)) {
                case ParseResult.Success<JValue> success -> {
                    switch (success.value()) {
                        case JObject jObject -> {
                            return new ParseResult.Success<>(jObject, success.next());
                        }
                        case JArray jArray -> {
                            return new ParseResult.Success<>(jArray, success.next());
                        }
                        default -> throw new RuntimeException("");
                    }
                }
                case ParseResult.Failure<JValue> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        }
    };

    Parser<JValue> jValueParser = new Parser<>() {
        Parser<JValue> parser;
        @Override
        public ParseResult<JValue> parse(String input, int location) {
            if (parser == null) {
                parser = jStringParer.or(jNumberParser).or(jBooleanParser).or(jNullParser).or(jArrayParser);
            }

            return parser.parse(input, location);
        }
    };


    Parser<JValue> jStringParer = new Parser<>() {
        Parser<String> parser = Parser.dquoteString();

        @Override
        public ParseResult<JValue> parse(String input, int location) {
            switch (parser.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JString(success.value()), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        }
    };

    Parser<JValue> jNumberParser = new Parser<>() {
        Parser<Integer> parser = Parser.integer();

        @Override
        public ParseResult<JValue> parse(String input, int location) {
            switch (parser.parse(input, location)) {
                case ParseResult.Success<Integer> success -> {
                    return new ParseResult.Success<>(new JNumber(success.value()), success.next());
                }
                case ParseResult.Failure<Integer> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        }
    };

    Parser<JValue> jBooleanParser = new Parser<>() {
        Parser<String> parser1 = Parser.string("true");
        Parser<JValue> parser2 = (input, location) -> {
            switch (parser1.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JBoolean(true), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };

        Parser<String> parser3 = Parser.string("false");
        Parser<JValue> parser4 = (input, location) -> {
            switch (parser3.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JBoolean(false), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
        Parser<JValue> parser = parser2.or(parser4);
        @Override
        public ParseResult<JValue> parse(String input, int location) {
            return parser.parse(input, location);
        }
    };

    Parser<JValue> jNullParser = new Parser<>() {

        Parser<String> parser1 = Parser.string("null");
        Parser<JValue> parser = (input, location) -> {
            switch (parser1.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JNull(), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
        @Override
        public ParseResult<JValue> parse(String input, int location) {
            return parser.parse(input, location);
        }
    };

    Parser<JValue> jArrayParser = new Parser<>() {
        Parser<List<JValue>> parser = jValueParser.array(Parser.string("["), Parser.string("]"), Parser.string(","));

        @Override
        public ParseResult<JValue> parse(String input, int location) {
            switch (parser.parse(input, location)) {
                case ParseResult.Success<List<JValue>> success -> {
                    return new ParseResult.Success<>(new JArray(success.value()), success.next());
                }
                case ParseResult.Failure<List<JValue>> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        }
    };

    Parser<JValue> jObjectParser = new Parser<>() {

        Parser<String> keyParser = Parser.dquoteString().withSkipSpace();
        Parser<String> colonParer = Parser.string(":").withSkipSpace();

        Parser<Tuple<String, JValue>> kvParser = new Parser<>() {
            Parser<Tuple<Tuple<String, String>, JValue>> keyValueParser = keyParser.and(colonParer).and(jValueParser);

            @Override
            public ParseResult<Tuple<String, JValue>> parse(String input, int location) {
                switch (keyValueParser.parse(input, location)) {
                    case ParseResult.Success<Tuple<Tuple<String, String>, JValue>> success -> {
                        return new ParseResult.Success<>(new Tuple<>(success.value().fst().fst(), success.value().snd()), success.next());
                    }
                    case ParseResult.Failure<Tuple<Tuple<String, String>, JValue>> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            }
        };

        Parser<List<Tuple<String, JValue>>> parser = kvParser.array(Parser.charP('{'), Parser.charP('}'), Parser.charP(','));
        @Override
        public ParseResult<JValue> parse(String input, int location) {
            switch (parser.parse(input, location)) {
                case ParseResult.Success<List<Tuple<String, JValue>>> success -> {
                    return new ParseResult.Success<>(new JObject(success.value().stream().collect(Collectors.toMap(Tuple::fst, Tuple::snd))), success.next());
                }
                case ParseResult.Failure<List<Tuple<String, JValue>>> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        }
    };

}
