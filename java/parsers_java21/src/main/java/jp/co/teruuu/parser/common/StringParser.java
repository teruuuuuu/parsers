package jp.co.teruuu.parser.common;

public class StringParser implements Parser<String> {
    final String literal;
    public StringParser(String literal) {
        this.literal = literal;
    }
    @Override
    public ParseResult<String> parse(String input, int location) {
        if (input.startsWith(literal, location)) {
            return new ParseResult.Success<>(literal, location + literal.length());
        } else {
            return new ParseResult.Failure<>(String.format("not (literal=[%s], loc=[%d]), input=%s", literal, location, input), location);
        }
    }
}
