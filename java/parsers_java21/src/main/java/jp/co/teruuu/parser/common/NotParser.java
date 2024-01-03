package jp.co.teruuu.parser.common;

public class NotParser<T> implements Parser<String> {
    private final Parser<T> parser;

    public NotParser(Parser<T> parser) {
        this.parser = parser;
    }

    @Override
    public ParseResult<String> parse(String input, int location) {
        if (input.length() == location) {
            return new ParseResult.Failure<>("reached end.", location);
        } else {
            switch (parser.parse(input, location)) {
                case ParseResult.Failure<T> a -> {
                    return new ParseResult.Success<>(input.substring(location, location + 1), location + 1);
                }
                case ParseResult.Success<T> success -> {
                    return new ParseResult.Failure<>("", location);
                }
            }
        }
    }
}
