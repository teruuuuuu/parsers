package jp.co.teruuu.parser.common;

import java.util.List;

public class DQuoteStringParser implements Parser<String> {
    private final Parser<String> dquoteParser;
    private final Parser<String> parser;
    public DQuoteStringParser() {
        this.dquoteParser = Parser.charP('"');
        Parser<List<String>> parser1 = Parser.escape().or(this.dquoteParser.not()).seq();

        this.parser = (input, location) -> {
            switch (parser1.parse(input, location)) {
                case ParseResult.Success<List<String>> success -> {
                    return new ParseResult.Success<>(String.join("", success.value()), success.next());
                }
                case ParseResult.Failure<List<String>> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };

    }

    @Override
    public ParseResult<String> parse(String input, int location) {
        ParseResult<String> dquoteResult = dquoteParser.parse(input, location);
        if (dquoteResult instanceof ParseResult.Failure<String> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        ParseResult.Success<String > dquoteSuccess = (ParseResult.Success<String>) dquoteResult;
        location = dquoteSuccess.next();

        ParseResult<String> parseResult = parser.parse(input, location);
        if (parseResult instanceof ParseResult.Failure<String> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        ParseResult.Success<String> parseSuccess = (ParseResult.Success<String>) parseResult;
        location = parseSuccess.next();

        dquoteResult = dquoteParser.parse(input, location);
        if (dquoteResult instanceof ParseResult.Failure<String> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        dquoteSuccess = (ParseResult.Success<String>) dquoteResult;
        location = dquoteSuccess.next();

        return new ParseResult.Success<>(parseSuccess.value(), location);
    }
}
