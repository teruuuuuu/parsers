package jp.co.teruuu.parser.json;

import jp.co.teruuu.common.Tuple;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;

import java.util.List;
import java.util.stream.Collectors;

public class JsonParser implements Parser<Json> {

    @Override
    public ParseResult<Json> parse(String input, int location) {
        return jsonParser.parse(input);
    }

    private Parser<Json> jsonParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<JValue> parser = jObjectParser.or(jArrayParser);
            return (input, location) -> {
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
            };
        }
    };

    private Parser<JValue> jValueParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            return jStringParer.or(jNumberParser).or(jBooleanParser).or(jNullParser).or(jArrayParser).or(jObjectParser);
        }
    };


    private Parser<JValue> jStringParer = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<String> parser = Parser.dquoteString();
            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<String> success -> {
                        return new ParseResult.Success<>(new JString(success.value()), success.next());
                    }
                    case ParseResult.Failure<String> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<JValue> jNumberParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<Integer> parser = Parser.integer();
            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<Integer> success -> {
                        return new ParseResult.Success<>(new JNumber(success.value()), success.next());
                    }
                    case ParseResult.Failure<Integer> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<JValue> jBooleanParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<JValue> trueParser = (input, location) -> {
                if (input.startsWith("true", location)) {
                    return new ParseResult.Success<>(new JBoolean(true), location + 4);
                } else {
                    return new ParseResult.Failure<>(String.format("not true (location=[%d], input=[%s])", location, input), location + 4);
                }
            };
            Parser<JValue> falseParser = (input, location) -> {
                if (input.startsWith("false", location)) {
                    return new ParseResult.Success<>(new JBoolean(false), location + 5);
                } else {
                    return new ParseResult.Failure<>(String.format("not false (location=[%d], input=[%s])", location, input), location + 4);
                }
            };
            return trueParser.or(falseParser);
        }
    };

    private Parser<JValue> jNullParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<String> parser = Parser.string("null");

            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<String> success -> {
                        return new ParseResult.Success<>(new JNull(), success.next());
                    }
                    case ParseResult.Failure<String> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<JValue> jArrayParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<List<JValue>> parser = jValueParser.array(Parser.string("["), Parser.string("]"), Parser.string(","));

            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<List<JValue>> success -> {
                        return new ParseResult.Success<>(new JArray(success.value()), success.next());
                    }
                    case ParseResult.Failure<List<JValue>> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<JValue> jObjectParser = new JsonParserBase<>() {
        @Override
        protected Parser<JValue> genParser() {
            Parser<String> keyParser = Parser.dquoteString().withSkipSpace();
            Parser<String> colonParer = Parser.string(":").withSkipSpace();
            Parser<JValue> jValueWithSkip = jValueParser.withSkipSpace();
            Parser<Tuple<String, JValue>> kvParser = new Parser<>() {
                Parser<Tuple<Tuple<String, String>, JValue>> keyValueParser = keyParser.and(colonParer).and(jValueWithSkip);

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
            Parser<List<Tuple<String, JValue>>> objectParser = kvParser.array(Parser.charP('{'), Parser.charP('}'), Parser.charP(','));
            return (input, location) -> {
                switch (objectParser.parse(input, location)) {
                    case ParseResult.Success<List<Tuple<String, JValue>>> success -> {
                        return new ParseResult.Success<>(new JObject(success.value().stream().collect(Collectors.toMap(Tuple::fst, Tuple::snd))), success.next());
                    }
                    case ParseResult.Failure<List<Tuple<String, JValue>>> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
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
