package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;

public interface JsonParser {


    static Parser<JString> jStringParer() {
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

    static Parser<JNumber> jNumberParser() {
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
    static Parser<JBoolean> JBooleanParser() {
        Parser<String> parser1 = Parser.string("true");
        Parser<JBoolean> parser2 = (input, location) -> {
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
        Parser<JBoolean> parser4 = (input, location) -> {
            switch (parser3.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JBoolean(false), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
        return parser2.or(parser4);
    }

    static Parser<JNull> JNullParser() {
        Parser<String> parser1 = Parser.string("null");
        return (input, location) -> {
            switch (parser1.parse(input, location)) {
                case ParseResult.Success<String> success -> {
                    return new ParseResult.Success<>(new JNull(), success.next());
                }
                case ParseResult.Failure<String> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
    }

    static Parser<JValue> jValueParser() {
        Parser<JString> jStringParser1 = jStringParer();
        Parser<JValue> jStringParser2 = (input, location) -> {
            switch (jStringParser1.parse(input, location)) {
                case ParseResult.Success<JString> success -> {
                    return new ParseResult.Success<>(success.value(), success.next());
                }
                case ParseResult.Failure<JString> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
        return jStringParser2;
    }
}
