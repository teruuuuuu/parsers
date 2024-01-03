package jp.co.teruuu.parser.common;

import java.util.Optional;

public class OptionalParser<T> implements Parser<Optional<T>> {
    private final Parser<T> parser;

    public OptionalParser(Parser<T> parser) {
        this.parser = parser;
    }

    @Override
    public ParseResult<Optional<T>> parse(String input, int location) {
        switch (parser.parse(input, location)) {
            case ParseResult.Success<T> success -> {
                return new ParseResult.Success<>(Optional.of(success.value()), success.next());
            }
            case ParseResult.Failure<T> failure -> {
                return new ParseResult.Success<>(Optional.empty(), location);
            }
        }
    }
}
