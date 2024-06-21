package jp.co.teruuu.parser.common;

public class EndParser implements Parser<Void> {
    @Override
    public ParseResult<Void> parse(String input, int location) {
        if (input.length() == location) {
            return new ParseResult.Success<>(null, location);
        } else {
            return new ParseResult.Failure<>(String.format("not eof expected length=[%d] actual=[%d]", location, input.length()), location);
        }
    }
}
