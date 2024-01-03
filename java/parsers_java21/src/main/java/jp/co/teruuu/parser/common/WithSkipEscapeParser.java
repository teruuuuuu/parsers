package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Tuple;

public class WithSkipEscapeParser<T> implements Parser<T> {
    private final Parser<Tuple<Void, T>> parser;

    public WithSkipEscapeParser(Parser<T> parser) {
        this.parser = new AndParser<>(new SkipSpaceParser(), parser);
    }

    @Override
    public ParseResult<T> parse(String input, int location) {
        switch (parser.parse(input, location)) {
            case ParseResult.Success<Tuple<Void, T>> success -> {
                return new ParseResult.Success<>(success.value().snd(), success.next());
            }
            case ParseResult.Failure<Tuple<Void, T>> failure -> {
                return new ParseResult.Failure<>(failure.message(), failure.next());
            }
        }
    }
}
