package jp.co.teruuu.parser.json;

import jp.co.teruuu.common.Either;
import jp.co.teruuu.common.Tuple;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;

import java.util.List;
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

    private Parser<Json> jNumberParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<Either<Integer,Double>> parser = Parser.number();
            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<Either<Integer,Double>> success -> {
                        return new ParseResult.Success<>(new JNumber(success.value()), success.next());
                    }
                    case ParseResult.Failure<Either<Integer,Double>> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<Json> jBooleanParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<Json> trueParser = (input, location) -> {
                if (input.startsWith("true", location)) {
                    return new ParseResult.Success<>(new JBoolean(true), location + 4);
                } else {
                    return new ParseResult.Failure<>(String.format("not true (location=[%d], input=[%s])", location, input), location + 4);
                }
            };
            Parser<Json> falseParser = (input, location) -> {
                if (input.startsWith("false", location)) {
                    return new ParseResult.Success<>(new JBoolean(false), location + 5);
                } else {
                    return new ParseResult.Failure<>(String.format("not false (location=[%d], input=[%s])", location, input), location + 4);
                }
            };
            return trueParser.or(falseParser);
        }
    };

    private Parser<Json> jNullParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
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

    private Parser<Json> jArrayParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<List<Json>> parser = jsonParser.array(Parser.string("["), Parser.string("]"), Parser.string(","));

            return (input, location) -> {
                switch (parser.parse(input, location)) {
                    case ParseResult.Success<List<Json>> success -> {
                        return new ParseResult.Success<>(new JArray(success.value()), success.next());
                    }
                    case ParseResult.Failure<List<Json>> failure -> {
                        return new ParseResult.Failure<>(failure.message(), failure.next());
                    }
                }
            };
        }
    };

    private Parser<Json> jObjectParser = new JsonParserBase<>() {
        @Override
        protected Parser<Json> genParser() {
            Parser<String> keyParser = Parser.dquoteString().withSkipSpace();
            Parser<String> colonParer = Parser.string(":").withSkipSpace();
            Parser<Json> jValueWithSkip = jsonParser.withSkipSpace();
            Parser<Tuple<String, Json>> kvParser = new Parser<>() {
                Parser<Tuple<Tuple<String, String>, Json>> keyValueParser = keyParser.and(colonParer).and(jValueWithSkip);

                @Override
                public ParseResult<Tuple<String, Json>> parse(String input, int location) {
                    switch (keyValueParser.parse(input, location)) {
                        case ParseResult.Success<Tuple<Tuple<String, String>, Json>> success -> {
                            return new ParseResult.Success<>(new Tuple<>(success.value().fst().fst(), success.value().snd()), success.next());
                        }
                        case ParseResult.Failure<Tuple<Tuple<String, String>, Json>> failure -> {
                            return new ParseResult.Failure<>(failure.message(), failure.next());
                        }
                    }
                }
            };
            Parser<List<Tuple<String, Json>>> objectParser = kvParser.array(Parser.charP('{'), Parser.charP('}'), Parser.charP(','));
            return (input, location) -> {
                switch (objectParser.parse(input, location)) {
                    case ParseResult.Success<List<Tuple<String, Json>>> success -> {
                        return new ParseResult.Success<>(new JObject(success.value().stream().collect(Collectors.toMap(Tuple::fst, Tuple::snd))), success.next());
                    }
                    case ParseResult.Failure<List<Tuple<String, Json>>> failure -> {
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
